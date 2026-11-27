/* The window host of the Android library test. NativeActivity's glue calls
 * android_main, which keeps the program's output in the app's files, runs the
 * program, and finishes the activity once it returns. */
#include <android_native_app_glue.h>
#include <fcntl.h>
#include <stdio.h>
#include <unistd.h>

extern void xwindow_main(void);

void android_main(struct android_app *app) {
    char path[512];
    snprintf(path, sizeof path, "%s/out.txt", app->activity->internalDataPath);
    int fd = open(path, O_WRONLY | O_CREAT | O_APPEND, 0600);
    if (fd >= 0) {
        dup2(fd, STDOUT_FILENO);
        dup2(fd, STDERR_FILENO);
    }
    xwindow_main();
    ANativeActivity_finish(app->activity);
    /* The glue waits on this thread for each lifecycle step of the activity. */
    while (!app->destroyRequested) {
        int events;
        struct android_poll_source *source = NULL;
        if (ALooper_pollOnce(-1, NULL, &events, (void **)&source) >= 0 && source)
            source->process(app, source);
    }
}
