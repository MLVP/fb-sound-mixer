'p=-gen gcc -O 3 -Wc -march=native,-mfpmath=both,-funroll-loops
#INCLUDE Once "Windows.bi"
#INCLUDE Once "win/mmsystem.bi"

'dim shared as any ptr libh
dim shared as integer ptr libh=0

type mp3dec_frame_info_t
	as long frame_bytes
	as long frame_offset
	as long channels
	as long hz
	as long layer
	as long bitrate_kbps
end type

Dim Shared mp3_info as function(dt as ubyte ptr, dt_len as long, info as mp3dec_frame_info_t ptr) as long
Dim Shared mp3_decode as function(dt as ubyte ptr, dt_len as long, info as mp3dec_frame_info_t ptr, pcm as ubyte ptr) as long

sub loadlib()
	if libh then DyLibFree (libh)
	
	libh        =   Dylibload ("lmp3.dll")
	mp3_info 	=	Dylibsymbol (libh,  "mp3_info")
	mp3_decode 	=	Dylibsymbol (libh,  "mp3_decode")
end sub

'-Wc -funroll-loops

enum pan_type
	pan_Simple = 0
	pan_Space1D = 1
	pan_Simple2D = 2
	pan_Head2D = 3
end enum

Type WAVE_fmt
    AudioFormat As UShort
    NumChannels As UShort
    SampleRate As Long
    ByteRate As Long
    BlockAlign As UShort
    BitsPerSample As UShort
End Type

Type WAVE_sample
    data(3) As Single PTR
    sample_count As Long
    sample_speed As Single
End Type

Type QueueWAVEHDR
    _wavehdr As WAVEHDR
    _next As QueueWAVEHDR PTR
End Type

Union WAVEDATA
    lp8 As UByte PTR
    lp16 As Short PTR
    'lp16 As Ushort Ptr
    lpAny As Any PTR
End Union

Enum WHDRFLAGS
    WHDR_DONE       = &H01 ' done bit
    WHDR_PREPARED   = &H02 ' set if this header has been prepared
    WHDR_BEGINLOOP  = &H04 ' loop start block
    WHDR_ENDLOOP    = &H08 ' loop end block
    WHDR_INQUEUE    = &H10 ' reserved for driver
    WHDR_VALID      = &H1F ' valid flags (Internal)
End Enum

Type WAVEHDR2 Field = 1
    lpData As WAVEDATA
    BufferLength As Integer
    BytesRecorded As Integer
    UserData As Integer
    Flags As WHDRFLAGS
    Loops As Integer
    lpNext As WAVEHDR PTR
    reserved As Integer
End Type



Dim Shared As HWAVEOUT _hWaveOut
Dim Shared pWave() As WAVEHDR PTR
Dim Shared bufferCount As Long 
Dim Shared CurrentBuffer As Long 
Dim Shared trackBuffer As Long
Dim Shared Frequency As Long
Dim Shared BufferSize As Long
Dim Shared BufferSize_s As Long

Dim Shared As HWAVEIN _hWaveIn
Dim Shared cs_in As CRITICAL_SECTION
Dim shared RecBuffer() As WAVEHDR2 
Dim Shared bufferCountIn As Long 
Dim Shared bufferCurrentIn As Long
Dim Shared InTrackBuffer As Long
Dim Shared InFrequency As Long



dim shared as single GlobalVolume
Dim Shared As Long 	ResampleQuality
Dim Shared As Long 	Dithering
dim shared as long 	Max_Active

Dim Shared As Short PTR 	track
Dim Shared As Single PTR 	track_s

dim shared as single ptr dithvals = 0
dim shared as long dithvalsi

dim shared StateOUT as long 
dim shared StateIN as long 

Type Sample
    sample_id As Long
    
    play_id As ulong ' playing sample counter
    play As Long
    
    cur_pnt As Long
    lop As Long
    
	PanType as long 
    Speed As Single
    Volume As Single
    Pan As Single
    panX As Single
    panY As Single
    
    EQ_Low As Single
    EQ_High As Single
    
    
    enabled As Long
End Type


Dim Shared sml() As WAVE_sample
Dim Shared smlc As Long

Dim Shared snd() As Sample
Dim Shared sndc As Long
Dim Shared sndid As uLong
'Dim Shared SleepTime As Long
Dim Shared PanType As Long

dim shared call_buffer_out as sub (byval buff as Short ptr, byval SampleCount as long, byval Channels as long, byval SampleRate as long)
dim shared call_buffer_in as sub (byval buff as Short ptr, byval SampleCount as long, byval Channels as long, byval SampleRate as long)

dim shared call_stop_snd as sub (byval snd_i as long)

'#DEFINE DEBUG

#ifdef DEBUG
sub log_add(txt as string)
	dim as long ff=freefile
	open "amixlog.txt" for append as #ff
		print #ff, txt
	close #ff
end sub
#endif


' Playing sound sample
Function AddSample() As Long
    sndid += 1
    If sndc Then
        For n As Long = 0 To sndc - 1
            If snd(n).enabled = 0 Then
                snd(n).play_id = sndid
                Return n
            End If
        Next
		if sndc>=Max_Active then return -1
		
        ReDim Preserve snd(sndc)
    Else
		ReDim snd(sndc)
    End If
    snd(sndc).play_id = sndid
    sndc = sndc + 1
    Return sndc - 1
End Function

function SampleFind(id as long) as long
	For n As Long = sndc - 1 To 0 step -1
		If snd(n).play_id = id Then Return n
	Next
	return -1
end function

' global sound
Function AddSound() As Long
    If smlc  Then
		ReDim Preserve sml(smlc)
    Else
        ReDim sml(smlc)
    End If
    smlc = smlc + 1
    Return smlc - 1
End Function



Function SeparateEQ(id As Long) As Long
    sml(id).data(2) = allocate(sml(id).sample_count * sizeof(Single))
    sml(id).data(3) = allocate(sml(id).sample_count * sizeof(Single))
    Dim As Long n, m, j
    Dim As Single c1, c2
    For n = 0 To sml(id).sample_count - 1
        c1 = 0: c2 = 0
        For m = - 15 To 16
            j = n + m
            If j < 0 Then j = 0
            If j > sml(id).sample_count - 1 Then j = sml(id).sample_count - 1
            c1 = c1 + sml(id).data(0)[j]
            c2 = c2 + sml(id).data(1)[j]
        Next
'        c1 = c1 * 0.03125 '/ 32
'        c2 = c2 * 0.03125 '/ 32
        sml(id).data(2)[n] = c1 * 0.03125
        sml(id).data(3)[n] = c2 * 0.03125
    Next
    For n = 0 To sml(id).sample_count - 1
        sml(id).data(0)[n] = sml(id).data(0)[n] - sml(id).data(2)[n]
        sml(id).data(1)[n] = sml(id).data(1)[n] - sml(id).data(3)[n]
    Next
    Return 0
End Function



' ResampleQuality
' Dithering
Function Dith(v As Single) As Short
	
	dithvalsi = (dithvalsi+1) mod 256
    If dithvals[dithvalsi] > v - Int(v) Then
        return( Int(v) )
    Else
        return( Int(v) + 1 )
    End If

'    If Rnd > v - Int(v) Then
'        Dith = Int(v)
'    Else
'        Dith = Int(v) + 1
'    End If
End Function

	


Sub PlaySND(i As Long)
    Dim As Long el, n, nl, tcur_pnt
    Dim id As Long, lop As Long
    Dim p As Single, pi As Long, v As Single, v1 As Single, v2 As Single, it As Single
    Dim As single vi
    Dim ip As Long
    Dim speed As Single
    Dim As Single h1, h2, nh, sx1, sy1, sx2, sy2, nx, ny, sc
    Dim As Single lopss1, lopss2, hpss1, hpss2
    
    Dim As Single chv1, chv2
    Select Case snd(i).PanType
    Case 0 ' Simple
        If snd(i).pan < 0 Then
            chv1 = snd(i).volume
            chv2 = snd(i).volume / ( - snd(i).pan + 1)
        ElseIf snd(i).pan > 0 Then
            chv1 = snd(i).volume / (snd(i).pan + 1)
            chv2 = snd(i).volume
        Else
            chv1 = snd(i).volume
            chv2 = snd(i).volume
        End If
        hpss1 = 1: hpss2 = 1
        
    Case 1 ' Space 1D
        chv1 = (snd(i).volume*1.25) / ((Abs( - 0.5 - snd(i).pan) + 0.2) )
        chv2 = (snd(i).volume*1.25) / ((Abs( 0.5 - snd(i).pan) + 0.2 ))
        hpss1 = 1: hpss2 = 1
        
    Case 2 ' Simple Space 2D
        sx1 = snd(i).panX - ( - 0.5)
        sy1 = snd(i).panY
        sx2 = snd(i).panX - (0.5)
        sy2 = snd(i).panY
        h1 = Sqr(sx1*sx1 + sy1*sy1)
        h2 = Sqr(sx2*sx2 + sy2*sy2)
        chv1 = snd(i).volume / (h1 + 0.1)
        chv2 = snd(i).volume / (h2 + 0.1)
        hpss1 = 1: hpss2 = 1
        
    Case 3 ' Head in Space 2D
        sx1 = (( - 0.5) - snd(i).panX )
        sy1 = snd(i).panY
        sx2 = ((0.5) - snd(i).panX )
        sy2 = snd(i).panY
        h1 = Sqr(sx1*sx1 + sy1*sy1)
        h2 = Sqr(sx2*sx2 + sy2*sy2)
        chv1 = snd(i).volume / (h1 + 0.1)
        chv2 = snd(i).volume / (h2 + 0.1)
        nh = Sqr(2)
        nx = ( 1) / nh
        ny = ( - 1 ) / nh
        sc = (( - 1)*sx1 + ( - 1)*sy1) / h1
        hpss1 = (2 + ny * sc) / 3
        chv1 = chv1* hpss1
        nx = ( - 1) / nh
        'ny = ( - 1 ) / nh
        sc = (( 1)*sx2 + ( - 1)*sy2) / h2
        hpss2 = (2 + ny * sc) / 3
        chv2 = chv2*hpss2
        
    End Select
    
	chv1 *= GlobalVolume
	chv2 *= GlobalVolume
	
    lopss1 = snd(i).eq_low
    lopss2 = snd(i).eq_low
    hpss1 = hpss1 * snd(i).eq_high
    hpss2 = hpss2 * snd(i).eq_high
    
    id = snd(i).sample_id
    lop = snd(i).lop
    speed = sml(id).sample_speed * snd(i).speed
    
    tcur_pnt = snd(i).cur_pnt
	
'    el = (sml(id).sample_count-1 - snd(i).cur_pnt)
'    If el / speed > trackBuffer - 1 Then
'        snd(i).cur_pnt = snd(i).cur_pnt + (trackBuffer * speed)
'        el = trackBuffer - 1
'    Else
'        ' end of this sample(last part)
'        snd(i).play = lop
'        If lop = 0 Then
'            snd(i).enabled = 0
'            snd(i).cur_pnt = 0
'			if call_stop_snd<>NULL then call_stop_snd(snd(i).play_id)
'        Else
'            snd(i).cur_pnt = trackBuffer - el / speed
'        End If
'        el = el/ speed - 1
'        If el <= 0 Then Exit Sub
'    End If
    el = (sml(id).sample_count-1 - snd(i).cur_pnt)/ speed
    If el > trackBuffer - 1 Then
        snd(i).cur_pnt = snd(i).cur_pnt + (trackBuffer * speed)
        el = trackBuffer - 1
    Else
        ' end of this sample(last part)
        snd(i).play = lop
        If lop = 0 Then
            snd(i).enabled = 0
            snd(i).cur_pnt = 0
			if call_stop_snd<>NULL then call_stop_snd(snd(i).play_id)
        Else
            snd(i).cur_pnt = trackBuffer - el '/ speed
        End If
        'el = el - 1
        If el <= 0 Then Exit Sub
    End If
	
    
    If lop = 1 Then
        If ResampleQuality Then

			For n = 0 To trackBuffer - 1
				p = (tcur_pnt + n*speed) Mod sml(id).sample_count
				ip = Int(p)
				it = p - ip
				
				v1 = sml(id).data(0)[ip]* hpss1 + sml(id).data(2)[ip] * lopss1
				v2 = sml(id).data(0)[ip + 1]* hpss1 + sml(id).data(2)[ip + 1] * lopss1
				vi = track_s[n Shl 1] + (v1 + it * (v2 - v1))*chv1
				If vi < - 32768 Then vi = - 32768
				If vi > 32767 Then vi = 32767
				track_s[n Shl 1] = vi

				v1 = sml(id).data(1)[ip]* hpss2 + sml(id).data(3)[ip] * lopss2
				v2 = sml(id).data(1)[ip + 1] * hpss2 + sml(id).data(3)[ip + 1] * lopss2
				vi = track_s[n Shl 1 + 1] + (v1 + it * (v2 - v1))*chv2
				If vi < - 32768 Then vi = - 32768
				If vi > 32767 Then vi = 32767
				track_s[n Shl 1 + 1] = vi
			Next

        Else

			For n = 0 To trackBuffer - 1
				p = (tcur_pnt + n*speed) Mod sml(id).sample_count
				ip = Int(p)
				vi = track_s[n Shl 1] + (sml(id).data(0)[ip]* hpss1 + sml(id).data(2)[ip] * lopss1)*chv1
				If vi < - 32768 Then vi = - 32768
				If vi > 32767 Then vi = 32767
				track_s[n Shl 1] = vi
				vi = track_s[n Shl 1 + 1] + (sml(id).data(1)[ip]* hpss2 + sml(id).data(3)[ip] * lopss2)*chv2
				If vi < - 32768 Then vi = - 32768
				If vi > 32767 Then vi = 32767
				track_s[n Shl 1 + 1] = vi
			Next
        
        End If
        
    Else
        
        If ResampleQuality Then
		
                For n = 0 To el
                    p = (tcur_pnt + n*speed)
                    ip = Int(p)
                    it = p - ip
                    
                    v1 = sml(id).data(0)[ip]* hpss1 + sml(id).data(2)[ip]* lopss1
                    v2 = sml(id).data(0)[ip + 1]* hpss1 + sml(id).data(2)[ip + 1]* lopss1
                    vi = track_s[n Shl 1] + (v1 + it * (v2 - v1))*chv1
                    If vi < - 32768 Then vi = - 32768
                    If vi > 32767 Then vi = 32767
                    track_s[n Shl 1] = vi
    
                    v1 = sml(id).data(1)[ip]* hpss2 + sml(id).data(3)[ip]* lopss2
                    v2 = sml(id).data(1)[ip + 1]* hpss2 + sml(id).data(3)[ip + 1]* lopss2
                    vi = track_s[n Shl 1 + 1] + (v1 + it * (v2 - v1))*chv2
                    If vi < - 32768 Then vi = - 32768
                    If vi > 32767 Then vi = 32767
                    track_s[n Shl 1 + 1] = vi
                Next

        Else

                For n = 0 To el
                    p = (tcur_pnt + n*speed)
                    ip = Int(p)
					'if ip>sml(id).sample_count-1 then ip=sml(id).sample_count-1
                    vi = track_s[n Shl 1] + (sml(id).data(0)[ip]* hpss1 + sml(id).data(2)[ip]* lopss1)*chv1
                    If vi < - 32768 Then vi = - 32768
                    If vi > 32767 Then vi = 32767
                    track_s[n Shl 1] = vi
                    vi = track_s[n Shl 1 + 1] + (sml(id).data(1)[ip]* hpss2 + sml(id).data(3)[ip]* lopss2)*chv2
                    If vi < - 32768 Then vi = - 32768
                    If vi > 32767 Then vi = 32767
                    track_s[n Shl 1 + 1] = vi
                Next

        End If
    End If
    
End Sub



Sub FillBuffer()

    Clear track_s[0], 0, BufferSize_s
	
    For i as long = 0 To sndc - 1
        If snd(i).enabled = 1 Then
            If snd(i).play = 1 Then
                PlaySND i
            End If
        End If
    Next

	if Dithering then
		for n as long = 0 to trackBuffer*2-1
			track[n] = Dith(track_s[n])		
		next
	else
		for n as long = 0 to trackBuffer*2-1
			track[n] = track_s[n]		
		next
	end if

	if call_buffer_out <> NULL then
		call_buffer_out(track, trackBuffer, 2, Frequency)
	end if
	
End Sub

Sub dsp_callback (_hWaveOut As HWAVEOUT, msg As UINT, instance As DWORD, p1 As DWORD, p2 As DWORD)
    If msg = WOM_DONE Then
		'waveOutPrepareHeader(_hWaveOut,  pWave(CurrentBuffer), sizeof(WAVEHDR))
		
		waveOutWrite(_hWaveOut, pWave(CurrentBuffer), sizeof(WAVEHDR))

		
		' fill next
		CurrentBuffer = (CurrentBuffer+1) mod bufferCount
		FillBuffer()
		memcpy( pWave(CurrentBuffer)->lpData, track, BufferSize  )
	else

    End If
End Sub



'Sub RECORDER(ByVal num As Any PTR)
'	Dim As WAVEHDR2 current =cast(WAVEHDR2, RecBuffer(bufferCurrentIn))
'	dim as long inRead, inWork
'	inRead = 0: inWork = 1
'	bufferCurrentIn = 0
'	Do while (StateIN)
'		if call_buffer_in<>NULL then call_buffer_in(RecBuffer(inWork).lpData.lp16, InTrackBuffer, 1, InFrequency)
'		While (RecBuffer(inRead).Flags AND WHDR_Done) = 0
'			Sleep 1 ' release 1 msec to system
'		Wend
'		RecBuffer(inRead).Flags = WHDR_PREPARED
'		Swap inRead, inWork
'		waveInAddBuffer(_hWaveIn, cast(WAVEHDR ptr, @RecBuffer(inRead)), Sizeof(WAVEHDR))
		
		
''		if call_buffer_in<>NULL then call_buffer_in(current.lpData.lp16, InTrackBuffer, 1, InFrequency)
''			
''		While (RecBuffer(bufferCurrentIn).Flags AND WHDR_Done) = 0
''			Sleep 1 ' release 1 msec to system
''		Wend
''		RecBuffer(bufferCurrentIn).Flags = WHDR_PREPARED
''		'Swap inRead, inWork
''		waveInAddBuffer(_hWaveIn, cast(WAVEHDR ptr, @RecBuffer(bufferCurrentIn)), Sizeof(WAVEHDR))
''		
''		bufferCurrentIn +=1
''		bufferCurrentIn = bufferCurrentIn mod bufferCountIn 
''		'bufferCurrentIn
''		current = RecBuffer(bufferCurrentIn)
'	Loop
'End Sub

Function sm_DeviceCount() As Long
    Return waveOutGetNumDevs()
End Function

Function sm_GetDevice(ByVal id As Long, ByRef dscr As zString PTR) As Long
    Dim As WAVEOUTCAPS OutCaps
    waveOutGetDevCaps(id, @OutCaps, SizeOf(WAVEOUTCAPS))
    MemCpy @dscr[0], @OutCaps.szPname[0], Len(OutCaps.szPname)
    Return 0
End Function


'function InitSoundIn(byval Dev as ulong, ByVal aFrequency As Integer, ByVal aBufferSize As Integer, ByVal aBufferCount As Long,ByVal CallBackIn As Long = 0) As Long
'	Dim As WAVEFORMATEX fmt
'	InTrackBuffer = aBufferSize
'	InFrequency = aFrequency
'	bufferCountIn = aBufferCount
	
'    With fmt
'        .wFormatTag = 1 'PCM
'        .nChannels = 1
'        .nSamplesPerSec = aFrequency
'        .wBitsPerSample = 16
'        .nBlockAlign = (16\8) * 1
'        .nAvgBytesPerSec = (16\8) * 1 * aFrequency
'        .cbSize = sizeof(WAVEFORMATEX)
'    End With
	
'	If _hWaveIn Then
'		waveInStop(_hWaveIn)
'		waveInClose(_hWaveIn)
'		_hWaveIn = NULL
'		DeleteCriticalSection(@cs_in)
'	End If
	
'	' Open device
'	If waveInOpen(@_hWaveIn, Dev, @fmt, CALLBACK_NULL, 0, 0) <> 0 Then return 1
'	'If waveInOpen(@_hWaveIn, Dev, @fmt, Cast(DWORD, @dsp_callback_in), Cast(DWORD, @FreeBufferCountIn), CALLBACK_FUNCTION) <> 0 Then return 1
'	InitializeCriticalSection(@cs_in)
	
'	' Prepare buffers
'	dim bf as WAVEHDR ptr
'	redim RecBuffer(bufferCountIn - 1)
'	For i as long = 0 To bufferCountIn -1
'		With RecBuffer(i)
'			.Flags=0
'			.BufferLength = fmt.nBlockAlign * aBufferSize
'			.lpData.lpAny = Callocate(.BufferLength) 
'		End With
'		bf = cast(WAVEHDR ptr, @RecBuffer(i))
'		If waveInPrepareHeader(_hWaveIn, bf, Sizeof(WAVEHDR)) <> 0 Then
'			Print "ERROR: can't prepare record buffer(" & Str(i) & ")!"
'			Deallocate RecBuffer(0).lpData.lpAny
'			Deallocate RecBuffer(1).lpData.lpAny
'			waveInClose(_hWaveIn)
'			return 2
'		End If
'		If waveInAddBuffer(_hWaveIn, bf, Sizeof(WAVEHDR)) <> 0 Then
'			Print "ERROR: can't add record buffer(" & Str(i) & ")!"
'			waveInUnPrepareHeader(_hWaveIn, bf, Sizeof(WAVEHDR))
'			waveInUnPrepareHeader(_hWaveIn, bf, Sizeof(WAVEHDR))
'			Deallocate RecBuffer(0).lpData.lpAny
'			Deallocate RecBuffer(1).lpData.lpAny
'			waveInClose(_hWaveIn)
'			return 3
'		End If
'	Next 
	
	
'	' Start
'	If waveInStart(_hWaveIn) <> 0 Then
'		Print "ERROR: can't start recording!"
'		For i as long = 0 To 1
'			bf = cast(WAVEHDR ptr, @RecBuffer(i))
'			waveInUnPrepareHeader (_hWaveIn, bf, Sizeof(WAVEHDR))
'			Deallocate RecBuffer(i).lpData.lpAny
'		Next 
'		waveInClose(_hWaveIn)
'		return 4
'	End If
'	call_buffer_in = CPTR(Any PTR, CallBackIn)
'	StateIN = 1
'	Dim As Any PTR thd = ThreadCREATE(@RECORDER)
'	return 0
'end function

'function StopSoundIn() as long
'	dim bf as WAVEHDR ptr
'	StateIN = 0
'	If _hWaveIn Then
'		waveInStop(_hWaveIn)
'		For i as long = 0 To 1
'			bf = cast(WAVEHDR ptr, bf)
'			waveInUnPrepareHeader (_hWaveIn, bf, Sizeof(WAVEHDR))
'			Deallocate RecBuffer(i).lpData.lpAny
'		Next
'		waveInClose(_hWaveIn)
'		_hWaveIn = NULL
'		return 0
'	End If
'	return 1
'end function

sub InitRNDs()
	if dithvals then deallocate(dithvals)
	dithvals = allocate(256 * sizeof(single))
	for n as long = 0 to 256-1
		dithvals[n] = rnd
	next
	dithvalsi = 0
end sub







extern "windows-ms"
	
	
	sub amix_INIT() export
		
		InitRNDs()
		
		loadlib()
		
		sndc=0
		sndid=0
		Max_Active=128
		
		GlobalVolume = 1.0
	end sub
	
	sub amix_SetMixerVolume(v as single) EXPORT
		GlobalVolume=v
		if GlobalVolume<0 then v=0
		if GlobalVolume>2 then GlobalVolume=2
	end sub

	function amix_StopOut() as long EXPORT
		StateOUT = 0
		If _hWaveOut Then
			for n as long = 0 to bufferCount-1
				waveOutUnPrepareHeader( _hWaveOut,  pWave(n), sizeof(WAVEHDR))
			next
			waveOutPause(_hWaveOut)
			waveOutReset(_hWaveOut)
			waveOutClose(_hWaveOut)
			_hWaveOut = NULL
			return 0
		End If
		return 1
	end function

	Function amix_InitOut(byval Dev as long, ByVal aFrequency As Integer, ByVal aBufferSize As Integer , ByVal aBufferCount As Long, byval Max_Samples as long=128, ByVal CallBackOut As Long = 0, ByVal CallBackStop As Long = 0) As Long EXPORT
		If aBufferSize Mod 4 <> 0 Then Return 4
		Frequency = aFrequency
		trackBuffer = aBufferSize
		bufferCount = aBufferCount
		BufferSize = 2 * trackBuffer * sizeof(short)
		BufferSize_s = 2 * trackBuffer * sizeof(single)
		track = allocate(BufferSize)
		track_s = allocate(BufferSize_s)

		Dim As WAVEFORMATEX wfx
		Dim devc As Long
		Dim ret As Long
		devc = waveOutGetNumDevs()
		If devc < 1 Then Return 2
		If _hWaveOut Then
			waveOutPause(_hWaveOut)
			waveOutReset(_hWaveOut)
			waveOutClose(_hWaveOut)
			_hWaveOut = NULL
		End If
		memset(@wfx, 0, sizeof(wfx))
		wfx.wFormatTag = WAVE_FORMAT_PCM
		wfx.nChannels = 2
		wfx.nSamplesPerSec = Frequency
		wfx.wBitsPerSample = 16
		wfx.nBlockAlign = wfx.nChannels * wfx.wBitsPerSample / 8
		wfx.nAvgBytesPerSec = wfx.nBlockAlign * wfx.nSamplesPerSec
		wfx.cbSize = 0

		If waveOutOpen(@_hWaveOut, Dev , @wfx, Cast(DWORD, @dsp_callback), 0 , CALLBACK_FUNCTION) = MMSYSERR_NOERROR Then
			waveOutRestart(_hWaveOut)
		Else
			_hWaveOut = NULL
			Return 1
		End If

		ReDim pWave(bufferCount - 1)
		For n As Long = 0 To bufferCount - 1
			pWave(n) = allocate(sizeof(QueueWAVEHDR))
			memset(pWave(n), 0, sizeof(WAVEHDR))
			pWave(n)->lpData = allocate(BufferSize)
			memset(pWave(n)->lpData, 0, BufferSize)
			pWave(n)->dwBufferLength = BufferSize '\ 2
			'pWave(n)->dwFlags = 0
			waveOutPrepareHeader(_hWaveOut,  pWave(n), sizeof(WAVEHDR))
		Next
		for n as long = 0 to bufferCount-1
			waveOutWrite(_hWaveOut,  pWave(n), sizeof(WAVEHDR))
		next

		
		call_buffer_out = CPTR(Any PTR, CallBackOut)
		call_stop_snd = CPTR(Any PTR, CallBackStop)
		StateOUT = 1
		sndid = 0
		Max_Active = Max_Samples
		Return 0
	End Function
		
	Function amix_sndPlay(sound_id As Long, lop As Long = 0, speed As Single = 1, volume As Single = 1, pan As Single = 0, pantype as pan_type = 0) As Long EXPORT
		Dim i As Long
		i = AddSample()
		if i=-1 then return -1
		snd(i).sample_id = sound_id
		snd(i).enabled = 1
		snd(i).play = 1
		snd(i).lop = lop
		snd(i).cur_pnt = 0
		snd(i).PanType = pantype	
		snd(i).Speed = speed
		snd(i).Volume = volume
		snd(i).Pan = pan
		snd(i).panx = 0
		snd(i).pany = 1
		snd(i).EQ_low = 1
		snd(i).EQ_high = 1
		Return( snd(i).play_id )
	End Function
	Function amix_sndPlaying(id As Long) as long EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return 0
		return snd(i).play
	End Function
	Function amix_sndExist(id As Long) as long EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return 0 else return 1
	End Function
	Sub amix_sndPause(id As Long) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).play = 0
	End Sub
	Sub amix_sndResume(id As Long) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).play = 1
	End Sub
	Sub amix_sndStop(id As Long) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).enabled = 0
	End Sub
	Sub amix_sndStopAll() EXPORT
		For n As Long = sndc - 1 To 0 step -1
			snd(n).enabled=0
		Next
	End Sub
	
	Sub amix_sndSetVolume(id As Long, volume as single) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).volume = volume
	End Sub	

	Sub amix_sndSetPan(id As Long, pan As Single = 0, panx As Single = 0, pany As Single = 1) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).Pan = pan
		snd(i).panx = panx
		snd(i).pany = pany
	End Sub	
	
	Sub amix_sndSetEQ(id As Long, low As Single = 0, high As Single = 1) EXPORT
		dim as long i = SampleFind(id)
		if i=-1 then return
		snd(i).EQ_low = low
		snd(i).EQ_high = high
	End Sub	
	
	Function amix_getActive(ids as ulong ptr) as long EXPORT
		dim as long c=0
		For n As Long = 0 To sndc - 1
			if snd(n).enabled then
				ids[c] = snd(n).play_id
				c+=1
			end if
		Next
		return c
	End Function
	
	
	Function amix_LoadWaveData(dt as ubyte ptr, DataLength as long, SampleRate as long, NumChannels as long, BitsPerSample as long, BlockAlign as long) As Long  EXPORT
		if DataLength<=0 then return -1
		if NumChannels<=0 then return -1
		if BlockAlign<1 then return -1
		if BitsPerSample<>8 and BitsPerSample<>16 then return -1
		
		dim as long i = AddSound()
		sml(i).sample_count = DataLength \ BlockAlign
		sml(i).data(0) = allocate(sml(i).sample_count * sizeof(Single))
		sml(i).data(1) = allocate(sml(i).sample_count * sizeof(Single))
		sml(i).sample_speed = SampleRate / Frequency
		dim as long j = 0
		For n as long = 0 To DataLength - 1 step BlockAlign
			Select Case BitsPerSample
			Case 8
				sml(i).data(0)[j] = (clng(dt[n])-127)*255
				If NumChannels = 2 Then
					sml(i).data(1)[j] = (clng(dt[n + 1])-127)*255
				End If
			Case 16
				sml(i).data(0)[j] = peek(Short, @dt[n])
				If NumChannels = 2 Then
					sml(i).data(1)[j] = peek(Short, @dt[n + 2])
				End If
			End Select
			j += 1
		Next
		If NumChannels = 1 Then
			' MONO -> STEREO
			For n as long = 0 To sml(i).sample_count - 1
				sml(i).data(1)[n] = sml(i).data(0)[n]
			Next
		End If
		SeparateEQ(i)
		return i
	end function
	
	Function amix_LoadMP3(fname As String) As Long  EXPORT

		dim as ubyte ptr dt
		dim as long ff = freefile, fl, i
		open fname for binary as #ff
			fl = LOF(ff)
			dt = allocate(fl)
			get #ff,1 , *dt, fl
		close #ff
		
		dim as mp3dec_frame_info_t 	info
		dim as long Samples, Smpl2
		
		'log_add "mp3 opening"
		
		Samples = mp3_info(dt, fl, @info)
		'log_add "  samples: " & Samples
		if Samples then

			dim as ubyte ptr pcm
			pcm = allocate( (Samples + 10 * 1152) * sizeof(short) * info.channels  )

			Smpl2 = mp3_decode(dt, fl, @info,  pcm)
			'log_add "  samples: " & Smpl2
			

			i = AddSound()
			sml(i).sample_count = Smpl2
			sml(i).data(0) = allocate(sml(i).sample_count * sizeof(Single))
			sml(i).data(1) = allocate(sml(i).sample_count * sizeof(Single))
			sml(i).sample_speed = info.hz / Frequency
			
			
			
			if info.channels=1 then
				for n as long = 0 to Smpl2-1
					sml(i).data(0)[n] = cast(short ptr, pcm)[n] 
					sml(i).data(1)[n] = cast(short ptr, pcm)[n] 
				next
			elseif info.channels=2 then
				for n as long = 0 to Smpl2-1
					sml(i).data(0)[n] = cast(short ptr, pcm)[n*2] 
					sml(i).data(1)[n] = cast(short ptr, pcm)[n*2+1] 
				next
			end if
			
			SeparateEQ(i)
			
			deallocate(pcm)
		end if
		
		deallocate(dt)
		return i
	end function


	Function amix_LoadWAV(fname As String) As Long EXPORT
		Dim ff As Long = freefile
		Dim p As Long
		Dim ChuckIDs As String*4
		Dim ChunkID As Long
		Dim ChunkSize As Long
		Dim FullChunkSize As Long
		Dim Format As Long
		Dim FileSize As Long
		Dim Samples As Long
		Dim ExtraParamSize As UShort
		Dim wValidBitsPerSample As UShort
		Dim dwChannelMask As Long
		Dim SubFormatGUID(16) As UByte
		Dim fmt As WAVE_fmt
		Dim i As Long
		i = -1
		p = 1
		Open fname For Binary As #ff
			FileSize = LOF(ff)
			Do
				Get #ff, p, ChunkID
				Get #ff, p + 4, ChunkSize
				Select Case ChunkID
				Case &H46464952
					'? "RIFF"
					If FileSize <> ChunkSize + 8 Then
						'? "Chunk size error"
					End If
					Get #ff, p + 8, Format
					If Format = &H45564157 Then
					Else
						'? "FORMAT ERROR: WAVE CHUNK"
						Return - 2
					End If
					p += 12
					
				Case &H20746d66
					'? "fmt "
					If ChunkSize >= 16 Then
						Get #ff, p + 8, fmt
'						Select Case fmt.AudioFormat
'						Case 0
'							? "format: Unknown"
'						Case 1
'							? "format: PCM"
'						Case 2
'							? "format: Microsoft ADPCM"
'						Case 6
'							? "format: ITU G.711 a-law"
'						Case 7
'							? "format: ITU G.711 µ-law"
'						Case 17
'							? "format: IMA ADPCM"
'						Case 20
'							? "format: ITU G.723 ADPCM (Yamaha)"
'						Case 49
'							? "format: GSM 6.10"
'						Case 64
'							? "format: ITU G.721 ADPCM"
'						Case 80
'							? "format: MPEG"
'						Case 65535
'							? "format: Experimental"
'						Case Else
'							? "format: unknown " & fmt.AudioFormat
'						End Select
						
	'    AudioFormat As UShort
	'    NumChannels As UShort
	'    SampleRate As Long
	'    ByteRate As Long
	'    BlockAlign As UShort
	'    BitsPerSample As UShort
'						? fmt.NumChannels
'						? fmt.SampleRate
'						? fmt.ByteRate
'						? fmt.BlockAlign
'						? fmt.BitsPerSample
						
						
						If ChunkSize > 16 Then
							Get #ff, p + 8 + 16, ExtraParamSize
							If ExtraParamSize >= 2 Then
								Get #ff, p + 8 + 16 + 2, wValidBitsPerSample
							End If
							If ExtraParamSize >= 6 Then
								Get #ff, p + 8 + 16 + 2 + 2, dwChannelMask
							End If
							If ExtraParamSize >= 22 Then
								Get #ff, p + 8 + 16 + 2 + 6, SubFormatGUID()
							End If
							
							'? "FORMAT ERROR: NOT PCM"
							Return - 3
						End If
					Else
						'? "FORMAT ERROR: fmt chunk size < 16"
					End If
					
				Case &H61746164
					'? "Data"
					Dim dt As UByte PTR
					Dim n As Long, j As Long
					dt = allocate(ChunkSize)
					Get #ff, p + 8 , dt[0], ChunkSize
					
					i = AddSound()
					sml(i).sample_count = ChunkSize \ fmt.BlockAlign
					sml(i).data(0) = allocate(sml(i).sample_count * sizeof(Single))
					sml(i).data(1) = allocate(sml(i).sample_count * sizeof(Single))
					sml(i).sample_speed = fmt.SampleRate / Frequency
					'? sml(i).sample_count
					j = 0
					For n = 0 To ChunkSize - 1 step fmt.BlockAlign
						Select Case fmt.BitsPerSample
						Case 8
							sml(i).data(0)[j] = (clng(dt[n])-127)*255
							If fmt.NumChannels = 2 Then
								sml(i).data(1)[j] = (clng(dt[n + 1])-127)*255
							End If
						Case 16
							sml(i).data(0)[j] = peek(Short, @dt[n])
							If fmt.NumChannels = 2 Then
								sml(i).data(1)[j] = peek(Short, @dt[n + 2])
							End If
						End Select
						j += 1
					Next
					If fmt.NumChannels = 1 Then
						' MONO -> STEREO
						For n = 0 To sml(i).sample_count - 1
							sml(i).data(1)[n] = sml(i).data(0)[n]
						Next
					End If
					
					SeparateEQ(i)
					
					deallocate dt

				Case &H74636166 ' fact
					Get #ff, p, ChuckIDs
					Get #ff, p + 8, Samples
					'? "fact"
				
				
				Case Else
					Get #ff, p, ChuckIDs
					'? "chunk id=" & ChuckIDs & "(" & ChunkID & ") "
					'? "size:" & ChunkSize
				End Select
				
				If ChunkID <> &H46464952 Then p += ((8 + ChunkSize + 1) \ 2) * 2
				If p > FileSize Then
					Exit Do
				End If
			Loop
		Close #ff
		
		Return i
	End Function
	
	sub amix_SetQuality(dither as long, resampling as long) EXPORT
		Dithering = dither
		ResampleQuality = resampling
	end sub

end extern


'dim shared inbuff as short ptr
'dim shared inbuff_state as long 

'dim shared ccnt(1) as long 

'sub WaveIn(byval buff as Short ptr, byval SampleCount as long, byval Channels as long, byval SampleRate as long)
''	memcpy inbuff, buff, SampleCount*2
''	inbuff_state = 1
''	ccnt(0) +=1
'end sub

'sub WaveOut(byval buff as Short ptr, byval SampleCount as long, byval Channels as long, byval SampleRate as long)
''	dim n as long
''	if inbuff_state then 
''		for n=0 to SampleCount-1
''			if abs(inbuff[n])>512 then 
''			buff[n*2] = inbuff[n] * 0.5
''			buff[n*2+1] = inbuff[n] * 0.5
''			end if
''		next
''		clear *inbuff,0, SampleCount*2
''		inbuff_state =0
''	else
''		windowtitle "" & rnd
''	end if
''	ccnt(1) +=1
'end sub



'ScreenRes 640, 480, 32
'amix_INIT()
'Dithering = 1
'ResampleQuality = 1


'? amix_InitOut(WAVE_MAPPER, 44100,  384, 8, 0) ' clng(@WaveOut)) ' 128:15 160:12 192:10  256:8	512:4  

'amix_LoadMP3( "veronika.mp3" )


''PlaySND 2, 0, 0
'Dim avg As Single, ky(9) As Long
'Dim As Long n, c
'Dim As Long mx, my, wl, bt, bto
'Dim As Single h1, h2, sx1, sy1, sx2, sy2, nx, ny, sc
'Dim echoc As Long , echot As Double

'dim as long i

'i = - 1
'While(1)

'	GetMouse mx, my, wl, bt
'	If bt=1 and bto = 0 Then

'		i = amix_PlaySample( 0, 0, 1 , 1 ,, pan_Head2D )

'	End If
'	bto=bt
	
'    Sleep 1,0'10
'Wend





