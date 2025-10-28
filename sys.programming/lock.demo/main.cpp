// Simple lock manager demo with S, IS, IX, SIX, X and compatibility matrix
#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>
#include <unistd.h>
#include <string.h>

typedef enum { IS=0, IX, S, SIX, X, LOCK_MODE_COUNT } lock_mode_t;

const char *mode_name[] = { "IS", "IX", "S", "SIX", "X" };

// compatibility[requesting][existing]
int compatibility[LOCK_MODE_COUNT][LOCK_MODE_COUNT] = {
    /* existing: IS IX  S  SIX X */
    /* IS */ { 1,  1,  1,  1,  0 },
    /* IX */ { 1,  1,  0,  0,  0 },
    /* S  */ { 1,  0,  1,  0,  0 },
    /* SIX*/ { 1,  0,  0,  0,  0 },
    /* X  */ { 0,  0,  0,  0,  0 }
};

#define MAX_LOCKS 32

typedef struct {
    pthread_t owner;
    lock_mode_t mode;
} held_lock_t;

typedef struct {
    const char *name;
    pthread_mutex_t mtx;
    pthread_cond_t  cv;
    held_lock_t held[MAX_LOCKS];
    int held_count;
} resource_t;

void resource_init(resource_t *r, const char *name) {
    r->name = name;
    pthread_mutex_init(&r->mtx, NULL);
    pthread_cond_init(&r->cv, NULL);
    r->held_count = 0;
}

int is_compatible(lock_mode_t req, resource_t *r) {
    for (int i = 0; i < r->held_count; ++i) {
        if (!compatibility[req][r->held[i].mode]) return 0;
    }
    return 1;
}

void acquire_lock(resource_t *r, lock_mode_t mode) {
    pthread_mutex_lock(&r->mtx);
    while (!is_compatible(mode, r)) {
        // wait until some other lock is released
        pthread_cond_wait(&r->cv, &r->mtx);
    }
    if (r->held_count < MAX_LOCKS) {
        r->held[r->held_count].owner = pthread_self();
        r->held[r->held_count].mode  = mode;
        r->held_count++;
        printf("[%lu] Acquired %s on %s (held=%d)\n",
               (unsigned long)pthread_self(), mode_name[mode], r->name, r->held_count);
    } else {
        fprintf(stderr, "lock table full\n");
    }
    pthread_mutex_unlock(&r->mtx);
}

void release_locks_by_owner(resource_t *r) {
    pthread_mutex_lock(&r->mtx);
    pthread_t me = pthread_self();
    int write_idx = 0;
    for (int i = 0; i < r->held_count; ++i) {
        if (!pthread_equal(r->held[i].owner, me)) {
            r->held[write_idx++] = r->held[i];
        } else {
            printf("[%lu] Released %s on %s\n", (unsigned long)me, mode_name[r->held[i].mode], r->name);
        }
    }
    r->held_count = write_idx;
    pthread_cond_broadcast(&r->cv);
    pthread_mutex_unlock(&r->mtx);
}

// Demo scenario:
// Reader: acquire IS on table, then S on row, hold, then release
// Writer: acquire IX on table, then X on row, hold, then release
resource_t table_res, row_res;

void *reader_thread(void *arg) {
    (void)arg;
    acquire_lock(&table_res, IS);
    usleep(100 * 1000); // simulate slight delay
    acquire_lock(&row_res, S);
    // simulate reading
    printf("[%lu] Reader: reading...\n", (unsigned long)pthread_self());
    sleep(2);
    // release row and table locks
    release_locks_by_owner(&row_res);
    release_locks_by_owner(&table_res);
    return NULL;
}

void *writer_thread(void *arg) {
    (void)arg;
    // start slightly after reader to show interleaving
    usleep(200 * 1000);
    acquire_lock(&table_res, IX);
    usleep(50 * 1000);
    acquire_lock(&row_res, X);
    printf("[%lu] Writer: writing...\n", (unsigned long)pthread_self());
    sleep(2);
    release_locks_by_owner(&row_res);
    release_locks_by_owner(&table_res);
    return NULL;
}

int main() {
    resource_init(&table_res, "table_users");
    resource_init(&row_res, "row_42");

    pthread_t r1, r2;
    pthread_create(&r1, NULL, reader_thread, NULL);
    pthread_create(&r2, NULL, writer_thread, NULL);

    pthread_join(r1, NULL);
    pthread_join(r2, NULL);

    printf("Done\n");
    return 0;
}
