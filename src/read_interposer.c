/* read() interposer.
 * Build and use this interposer as follows:
 * gcc -nostartfiles -shared -fPIC -o read_interposer.so read_interposer.c -ldl
 * export LD_PRELOAD=$cwd/read_interposer.so
 * Run the app
 * unset LD_PRELOAD
 */

#define _GNU_SOURCE
#include <stdio.h>
#include <dlfcn.h>
#include <unistd.h>
#include <assert.h>
#include <arpa/inet.h>

void _init()
{
    printf("Loaded library...~n");
}

void _fini()
{
    printf("Unloaded library...~n");
}

ssize_t read(int fd, void *buf, size_t count)
{
    printf("Read %d, %p, %u\n", fd, buf, count);
    static ssize_t (*func)(int , void*, size_t)=NULL;
    size_t result=-1;

    if (NULL == func)
    {
        func=(ssize_t (*)())dlsym(RTLD_NEXT, __func__);
        assert(NULL != func);
    }

    if (0==fd)
    {
        static uint32_t netlong = 0;
        static uint32_t expectedBytes = 0;
        static size_t readSoFar = 0;

        if (0 == netlong) // We have not read the length
        {
            // Read in network byte order
            ssize_t r = func(fd, &netlong, 4);
            assert(4 == r);
            // Convert to host byte order
            expectedBytes = ntohl(netlong);
            printf("Expecting %u bytes\n", expectedBytes);
        }

        if (readSoFar < expectedBytes)
        {
            ssize_t numToRead = count;
            if (expectedBytes - readSoFar < count)
                numToRead = expectedBytes - readSoFar;

            result = func(fd, buf, numToRead);
            if (0 < result)
                readSoFar += result;

            printf("Read %u\n", result);
        }
        else
        {
//            close(fd);
            result = 0;
        }
    }
    else
        result = func(fd, buf, count);

    return result;
} /* read */
