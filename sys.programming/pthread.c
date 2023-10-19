#include <stdio.h> 
#include <unistd.h> 
#include <pthread.h> 

typedef struct shared_buffer { 
  pthread_mutex_t lock; 
  pthread_cond_t cond; 
} shared_buffer_t;

void sb_init(shared_buffer_t *); 
void sb_destroy(shared_buffer_t *); 
void * producer(void *); 

void sb_init(shared_buffer_t *sb) 
{
  pthread_mutex_init(&sb->lock, NULL);
  pthread_cond_init (&sb->cond, NULL);
}

void sb_destroy(shared_buffer_t *sb) 
{
  pthread_mutex_destroy(&sb->lock); 
  pthread_cond_destroy (&sb->cond); 
}

int iFlag1 = 1;
int iFlag2 = 0;

void* producer(void * info)
{
  shared_buffer_t *sb = (shared_buffer_t *)info; 
  int i = 0;
  while (i < 100000) 
  {
    pthread_mutex_lock(&sb->lock); 
    while (iFlag1 == 0) 
      pthread_cond_wait(&sb->cond, &sb->lock); 

    printf("[%d]producer\n", i++); 
    iFlag1 = 0; 
    iFlag2 = 1; 
    pthread_cond_signal(&sb->cond); 
    pthread_mutex_unlock(&sb->lock); 
  }
} 

int main(int argc, char ** argv) 
{ 
  pthread_t th1; 
  shared_buffer_t sb; 

  sb_init(&sb); 
  pthread_create(&th1, NULL, &producer, (void *)&sb); 

  int i = 0; 
  while (i < 100000) 
  {
    pthread_mutex_lock(&sb.lock); 
    while (iFlag2 == 0) 
      pthread_cond_wait(&sb.cond, &sb.lock); 

    printf("[%d]consumer\n", i++); 
    iFlag1 = 1; 
    iFlag2 = 0; 
    pthread_cond_signal(&sb.cond); 
    pthread_mutex_unlock(&sb.lock); 
  } 
  pthread_join(th1, NULL); 
  sb_destroy(&sb); 

  return 0; 
} 
