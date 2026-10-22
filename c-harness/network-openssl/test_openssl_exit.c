/* Regression: OpenSSL's automatic process cleanup must not free its
 * global thread registry while GHC worker pthreads run TLS destructors.
 * Each child exits as its TLS-using workers exit, matching RTS shutdown. */
#include "harness.h"
#include "api.h"
#include <pthread.h>
#include <sys/wait.h>

#define WORKERS 16
static pthread_mutex_t lock = PTHREAD_MUTEX_INITIALIZER;
static pthread_cond_t changed = PTHREAD_COND_INITIALIZER;
static int ready;
static int go;

static void *tls_worker(void *unused)
{
    (void)unused;
    SSL_CTX *ctx = wf_ssl_ctx_new_client(0);
    if (!ctx) _exit(2);
    wf_ssl_ctx_free(ctx);
    /* Ensure OpenSSL has per-thread resources to destroy. */
    ERR_raise(ERR_LIB_USER, 1);
    pthread_mutex_lock(&lock);
    ++ready;
    pthread_cond_broadcast(&changed);
    while (!go) pthread_cond_wait(&changed, &lock);
    pthread_mutex_unlock(&lock);
    return NULL;
}

static void exit_with_tls_workers(void)
{
    pid_t pid = fork();
    WF_CHECK(pid >= 0);
    if (pid == 0) {
        alarm(10);
        wf_ssl_init();
        pthread_t workers[WORKERS];
        for (int i = 0; i < WORKERS; ++i)
            if (pthread_create(&workers[i], NULL, tls_worker, NULL) != 0)
                _exit(3);
        pthread_mutex_lock(&lock);
        while (ready != WORKERS) pthread_cond_wait(&changed, &lock);
        go = 1;
        pthread_cond_broadcast(&changed);
        pthread_mutex_unlock(&lock);
        /* Deliberately do not join: the RTS also exits with worker threads
         * still alive. Let their destructors race the process exit path. */
        exit(0);
    }
    int status;
    WF_CHECK(waitpid(pid, &status, 0) == pid);
    WF_CHECK(WIFEXITED(status));
    WF_CHECK_EQ(WEXITSTATUS(status), 0);
}

int main(void)
{
    for (int i = 0; i < 32; ++i) WF_RUN(exit_with_tls_workers);
    return wf_finish("test_openssl_exit");
}
