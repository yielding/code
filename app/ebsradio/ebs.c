/*
 * experimental hack
 * bushi at mizi dot com
 *
 * gcc -O2 -s -o ebs ebs_endianfree.c -Wall
 * ./ebs > a.bin
 * mplayer a.bin
 *
 * Note : a.bin is in mp3 format.
 *
 * $Id: ebs.c 15 2010-09-02 01:23:43Z orchistro $
 */

#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>
#include <unistd.h>
#include <signal.h>
#include <errno.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <sys/time.h>
#include <netinet/tcp.h>
#include <netinet/ip.h>
#include <arpa/inet.h>
#include <pthread.h>

#ifdef __MACH__     /* OSX */
#include <architecture/byte_order.h>
#else
#include <endian.h>
#include <asm/byteorder.h>
#endif

#include "ebs.h"

#define SERVER_IP "219.240.12.254"
#define CTL_PORT  5056
#define DATA_PORT 5057
#define PING_INTERVAL (3)

static const unsigned char packet_0[288] = {
    PACKET_0
};

static const unsigned char packet_1[288] = {
    PACKET_1
};

static const unsigned char packet_2[288] = {
    PACKET_2
};

static const unsigned char packet_3[288] = {
    PACKET_3
};

static unsigned char req_tok1[4] = {0,0,0,0};

/*
 * experimental, FIXME
 */
#define    EBS_HDR_MAGIC    0x0f0f0f0f
struct ebs_header_s {
    unsigned int magic; /* 0x0f0f0f0f */
    unsigned int unknown_1[3];
    unsigned short unknown_2;
    unsigned short this_len;
    unsigned int unknown_3[3];
}; /* 32 bytes */

struct context_s;

#define ST_ERROR (-1)
#define ST_IDLE (0)
#define ST_CTL_START (1)
#define ST_DATA_START (1)
#define ST_CTL_PREPARE_REQ (2)
#define ST_DATA_LOOP (2)
#define ST_CTL_MAKE_REQ_1 (3)
#define ST_CTL_MAKE_REQ_2 (4)
#define ST_CTL_MAKE_REQ_3 (5)
#define ST_CTL_LOOP (6)
#define ST_CTL_SEND_REQ (7)

typedef struct context_s {
    int ctrl_sock;
    int data_sock;
} context_t;

FILE         *outfile = NULL;
int          duration = 0;
struct timeval     end_tv;
context_t     *context = NULL;
volatile int     running = 0;

/* FUNCTION DECLARATIONS */
void destroy_context();

/**
 */
int make_sock(const char *ipaddr, int port)
{
    int sockfd;
    struct sockaddr_in servaddr;
    int ret;

    ret = socket(AF_INET, SOCK_STREAM, 0);
    if (ret < 0) {
        perror("socket()");
        return ret;
    }

    sockfd = ret;

    bzero(&servaddr, sizeof(servaddr));
    servaddr.sin_family = AF_INET;
    servaddr.sin_port = htons(port);

    ret = inet_pton(AF_INET, ipaddr, &servaddr.sin_addr);
    if (ret < 0) {
        perror("inet_pton()");
        close(sockfd);
        return ret;
    }
    if (ret == 0) {
        fprintf(stderr, "invalid address\n");
        close(sockfd);
        return -1;
    }

    ret = connect(sockfd, (struct sockaddr *)&servaddr, sizeof(servaddr));
    if (ret < 0) {
        perror("connect()");
        close(sockfd);
        return ret;
    }

    return sockfd;    
}

/* ------------- DATA SESSION ------------- */

#define    DATA_BUF_SIZE    (64 * 1024)
unsigned char packet_r[288] = { PACKET_R };
unsigned char *data_buf;

int write_data_packet_r(int fd)
{
    return write(fd, packet_r, sizeof(packet_r));
}

int read_data_dummy(int fd)
{
    return read(fd, data_buf, DATA_BUF_SIZE);
}

int parse_ebs_hdr(struct ebs_header_s *ebs_hdr)
{
    int len;
    /* check if the header is alright.. */
    if (ebs_hdr->magic != EBS_HDR_MAGIC) {
        fprintf(stderr, "EBS Header is invalid\n");
        return -EINVAL;
    }
#ifdef __MACH__     /* OSX */
    len = OSSwapLittleToHostInt16(ebs_hdr->this_len);
#else
    len = __le16_to_cpu(ebs_hdr->this_len);
#endif
    fprintf(stderr, "DATA : %d\n", len);
    return len;
}

/**
 * Main purpose of read_and_find_ebs_hdr function is
 * findind EBS_HDR_MAGIC inside the audio stream
 * when you lost ebs header while you are reading from the audio stream,
 *
 * This function find ebs header and writes the data after the header.
 * And finally returns the length of data left to read.
 *
 * This function is coded but not used yet.
 * I think, the remaining of the code will work well without help of
 * this code ^^.
 */
int read_and_find_ebs_hdr(int fd)
{
    int i;
    int ret, len, left;
    int found = -1;
    int magic = EBS_HDR_MAGIC;

    do {
        ret = read(fd, data_buf, DATA_BUF_SIZE);
        if (ret <= 0)
            return ret;

        for (i = 0; i < ret - 4; i ++) {
            if (!memcmp(data_buf + i, (void *)&magic, 4)) {
                found = i;
                break;
            }
        }
    } while(found < 0);

    // this must return valid value
    left = parse_ebs_hdr((struct ebs_header_s *)(data_buf + i));

    if (ret - found > 0) {
        len = ret - found;
        ret = fwrite(data_buf + found, 1, len, outfile);
        if (ret < 0) {
            fprintf(stderr, "Cannot write to output file\n");
            outfile = NULL;
            return -EIO;
        }
        fflush(outfile);
        left -= len;
    }
    return left;
}

int read_data_ebs_hdr(int fd)
{
    int ret, hdr_len = 0;
    struct ebs_header_s ebs_hdr;
    void *ptr = &ebs_hdr;

    ret = read(fd, ptr, sizeof(ebs_hdr));
    if (ret <= 0)
        return ret;

    /* if not all of the header is not received.. */
    hdr_len = ret;
    while (hdr_len < sizeof(ebs_hdr)) {
        ret = read(fd, ptr + hdr_len, sizeof(ebs_hdr) - hdr_len);
        if (ret <= 0)
            return ret;
        hdr_len += ret;
    }

    return parse_ebs_hdr(&ebs_hdr);
}

/* MPEG ADTS, layer III, v2,  64 kBits, 24 kHz, JointStereo */
// static const unsigned char mp3_magic[4] = { 0xff, 0xf3, 0x84, 0x64 };
static const unsigned char mp3_magic[4] = { 0xff, 0xfb, 0x94, 0x64 };
int read_data_preamble(int fd, int leng) {
    int ret, found, i;
    int pos = 0, left = leng;

    /* read remaining of the packet */
    while (left) {
        ret = read(fd, data_buf + pos, left);
        if (ret < 0)
            return ret;
        
        left -= ret;
        pos += ret;
    }

    /* find MP3 preamble -- FIXME: validate properly */
    found = -1;
    for (i = 0; i < leng - 4; i ++) {
        if (!memcmp(data_buf + i, mp3_magic, 4)) {
            found = i;
            break;
        }
    }

    if (found < 0) {
        fprintf(stderr, "MP3 preamble is not found\n");
        return -EAGAIN;
    }

    fprintf(stderr, "MP3 preamble : @%d\n", found);

    /* Write the mp3 content to output file */
    ret = fwrite(data_buf + found, 1, leng - found, outfile);
    if (ret <= 0) {
        fprintf(stderr, "Cannot write to output file\n");
        outfile = NULL;
        return -EIO;
    }

    fflush(outfile);
    return leng - found;
}

int read_data_mp3(int fd, int leng) {
    int left = leng;
    int ret, size;

    while (left > 0) {
        /* read mp3 packet from stream */
        size = (left < DATA_BUF_SIZE) ? left : DATA_BUF_SIZE;
        ret = read(fd, data_buf, left);
        if (ret <= 0)
            return ret;
        left -= ret;

        /* write the mp3 content to output file */
        size = ret;
        ret = fwrite(data_buf, 1, size, outfile);
        if (ret < 0) {
            fprintf(stderr, "Cannot write to output file\n");
            outfile = NULL;
            return -EIO;
        }
        fflush(outfile);
    }

    return 0;
}

void* data_thr(void *targ)
{
    int ret, len;
    int sock;

    /* allocate data buffer for communication */
    data_buf = malloc(DATA_BUF_SIZE);
    if (data_buf == NULL) {
        fprintf (stderr, "Error allocating data buffer\n");
        return (void *)0xDEADBEEF;
    }

    /* Open data socket */
    fprintf (stderr, "[DATA] Make socket\n");
    sock = make_sock(SERVER_IP, DATA_PORT);
    if (sock < 0) {
        fprintf (stderr, "Error openning data session\n");
        return (void *)0xDEADBEEF;
    }

    /* Write a packet with the token : to get data */
    memcpy(packet_r + 20, req_tok1, 4);
    fprintf (stderr, "[DATA] Write packet_r\n");
    ret = write_data_packet_r(sock);
    if (ret < 0) goto err;
    ret = read_data_dummy(sock);
    if (ret < 0) goto err;

    /* Get MP3 preamble */
    while (running) {
        sleep(1);    // wait for 1 sec

        fprintf (stderr, "[DATA] Get MP3 Preamble\n");
        len = read_data_ebs_hdr(sock);
        if (len <= 0) goto err;
        ret = read_data_preamble(sock, len);
        if (ret == -EAGAIN) {
                   continue;
        } else if (ret < 0) {
               goto err;
        } else 
          break;
    }
    
    /* Get MP3 packets while there is no error */
    while (running) {
        len = read_data_ebs_hdr(sock);
        if (len <= 0) break;
        ret = read_data_mp3(sock, len);
        if (ret < 0) break;
    }

err:
    running = 0;
    close(sock);
    return NULL;
}

/* ------------- CONTROL SESSION ------------- */

int ctrl_send_ping(int fd)
{
    return write(fd, packet_0, sizeof(packet_0));
}

int ctrl_send_prep1(int fd)
{
    return write(fd, packet_1, sizeof(packet_1));
}

int ctrl_send_prep2(int fd)
{
    return write(fd, packet_2, sizeof(packet_2));
}

int ctrl_send_ping2(int fd)
{
    return write(fd, packet_3, sizeof(packet_3));
}

unsigned char ctrl_buf[300];    // original : 288

int ctrl_read_ebs_radio(int fd)
{
    int ret;
    ret = read(fd, ctrl_buf, sizeof(ctrl_buf));
    if (ret > 0) {
        fprintf(stderr, "[%s]\n", &ctrl_buf[36]);
        // TODO check if it is "EBS RADIO"
    }
    return ret;
}

int ctrl_read_token(int fd) {
    int ret = read(fd, ctrl_buf, sizeof(ctrl_buf));
    if (ret > 0) {
        memcpy(req_tok1, ctrl_buf + 20, 4);
        fprintf(stderr, "REQUEST TOKEN : 0x%02x 0x%02x 0x%02x 0x%02x\n",
                req_tok1[0], req_tok1[1],
                req_tok1[2], req_tok1[3]);
    }

    return ret;
}

int ctrl_read_dummy(int fd) {
    return read(fd, ctrl_buf, sizeof(ctrl_buf));
}

#define    TRYTO(x)   if ((x) < 0) goto err;

/*
 */
void proc_ctrl_session()
{
    int ret;
    struct timeval now_tv;
    pthread_t thr;
    void *pret;

    int sock = make_sock(SERVER_IP, CTL_PORT);
    if (sock < 0)
        return;

    fprintf(stderr, "[CTRL] Start\n");
    TRYTO(ret = ctrl_send_ping(sock));
    TRYTO(ret = ctrl_read_ebs_radio(sock));

    fprintf(stderr, "[CTRL] Get Token\n");
    TRYTO(ret = ctrl_send_prep1(sock));
    TRYTO(ret = ctrl_read_token(sock));

    fprintf(stderr, "[CTRL] Dummy\n");
    TRYTO(ret = ctrl_send_prep2(sock));
    TRYTO(ret = ctrl_read_dummy(sock));

    /* make data thread */
    running = 1;
    fprintf(stderr, "[CTRL] Make data thread\n");
    pthread_create(&thr, NULL, data_thr, NULL);

    while (running) {    // PING
        gettimeofday(&now_tv, NULL);
        if (duration && (now_tv.tv_sec > end_tv.tv_sec)) {
            running = 0;
            break;
        }

        /* fprintf(stderr, "[CTRL] Ping\n"); */
        ret = ctrl_send_ping(sock);
        if (ret < 0) {
            running = 0;
            break;
        }

        TRYTO(ret = ctrl_read_ebs_radio(sock));
        TRYTO(ret = ctrl_send_ping2(sock));
        TRYTO(ret = ctrl_read_dummy(sock));
        TRYTO(ret = ctrl_read_dummy(sock));

        sleep(PING_INTERVAL);
    }
err:
    running = 0;
    pthread_join(thr, &pret);
    if (pret == (void *)0xDEADBEEF) {
        // NOT important
    }
    close(sock);
}

/*
 * -----------------------------------------------------------------------------
 *  Processing commandline arguments.
 *  Initializing duration variale and setting time limit.
 * -----------------------------------------------------------------------------
 */
void process_args(int argc, char **argv)
{
    if (argc >= 2)
    {
        outfile = fopen(argv[1], "wb");

        if (outfile == NULL)
        {
            perror(argv[1]);
            exit(errno);
        }
        else
        {
            /* fopen success. Nothing to do */
        }
    }

    if (argc >= 3)
    {
        duration = atol(argv[2]);
        if (duration <= 0)
        {
            duration = 0;
        }
        else 
        {
            gettimeofday(&end_tv, NULL);
            end_tv.tv_sec += duration * 60;
        }
    }

    if (outfile == NULL)
        outfile = stdout;
}

////////////////////////////////////////////////////////////////////////////////
//
// USAGE : ebs <out-file> <duration>
//
////////////////////////////////////////////////////////////////////////////////
int main(int argc, char **argv)
{
  struct timeval now_tv;

  process_args(argc, argv);

  while (1) {
    /* repeat until determined time comes ... */
    proc_ctrl_session();

    if ((duration > 0)) {
      /* only if <duration> is specified */
      gettimeofday(&now_tv, NULL);

      if (now_tv.tv_sec > end_tv.tv_sec) break;
    }
  };

  fclose(outfile);

  return EXIT_SUCCESS;
}
