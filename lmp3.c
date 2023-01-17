// gcc.exe -shared "D:\VBProjects\MEDIA\mp3\minimp3-master\mp3_decode_test.c" -o "D:\VBProjects\MEDIA\mp3\minimp3-master\test.dll"
#define MINIMP3_IMPLEMENTATION
//#define MINIMP3_NO_SIMD
#define MINIMP3_ONLY_MP3
#include "minimp3.h"


/*
typedef struct
{
    int frame_bytes, frame_offset, channels, hz, layer, bitrate_kbps;
} mp3dec_frame_info_t;
*/
int mp3_info(uint8_t *data, int data_size, mp3dec_frame_info_t *info) {
    mp3dec_t                mp3d;

    memset(info, 0, sizeof(*info));

    uint8_t *cbuf = data;

    int smpl_tot=0, ghdr=0;
    do
    {
        int free_format_bytes = 0, frame_size = 0, ret;
        int i = mp3d_find_frame(cbuf, data_size, &free_format_bytes, &frame_size);

        cbuf      += i;
        data_size -= i;

        if (i && !frame_size) continue;
        if (!frame_size) break;

        const uint8_t *hdr = cbuf;
        if (ghdr==0) {
            ghdr=1;
            (*info).channels = HDR_IS_MONO(hdr) ? 1 : 2;
            (*info).hz = hdr_sample_rate_hz(hdr);
            (*info).layer = 4 - HDR_GET_LAYER(hdr);
            (*info).bitrate_kbps = hdr_bitrate_kbps(hdr);
            (*info).frame_bytes = frame_size;
        }
        smpl_tot += hdr_frame_samples(hdr);

        cbuf      += frame_size;
        data_size -= frame_size;
    } while (1);

    return (smpl_tot);
}

int mp3_decode(uint8_t *data, int data_size, mp3dec_frame_info_t *info, uint8_t *PCM) {
    mp3dec_t                mp3d;
    unsigned char           *cbuf;

    mp3dec_init(&mp3d);
    cbuf = data;
    int buf_size = data_size;
    //memset(info, 0, sizeof(mp3dec_frame_info_t));
    //mp3dec_decode_frame(&mp3d, cbuf, buf_size, 0, info);

    short pcm[MINIMP3_MAX_SAMPLES_PER_FRAME];
    int frame_size;
    int smpl_tot=0, ghdr=0;
    int samples;
    do
    {
        samples = mp3dec_decode_frame(&mp3d, cbuf, buf_size, pcm, info);

        if (samples) {
            memcpy ( PCM + smpl_tot*sizeof(short) * (*info).channels, pcm, samples*sizeof(short)*(*info).channels );
            smpl_tot += samples;
        }

        cbuf = cbuf + (*info).frame_bytes;
        buf_size = buf_size - (*info).frame_bytes;
        //frame_size = (*info).frame_bytes - (*info).frame_offset;

        if ((*info).frame_bytes==0 && samples==0) break;

    } while (1); // ((*info).frame_bytes);

    return(smpl_tot);
}






