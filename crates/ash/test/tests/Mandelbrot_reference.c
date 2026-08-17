// Faithful C port of crates/ash/test/tests/Mandelbrot.hx — used to establish
// ground truth for float-contraction (FMA) policy across ash's engines.
// The fusable sites are complexLength2 (i*i + j*j) and complexSquare
// (i*i - j*j); everything else is plain mul/add/sub.
#include <stdio.h>
#include <math.h>

#define SIZE 25
#define MAX_ITERATIONS 1000
#define MAX_RAD (1 << 16)

#ifndef WIDTH
#define WIDTH 875
#endif
#ifndef HEIGHT
#define HEIGHT 500
#endif

typedef struct { int r, g, b; } RGB;
typedef struct { double i, j; } Complex;

static RGB create_palette(double in_fraction) {
    RGB c;
    c.r = (int)(in_fraction * 255);                                  // Std.int truncates
    c.g = (int)((1 - in_fraction) * 255);
    c.b = (int)((0.5 - fabs(in_fraction - 0.5)) * 2 * 255);
    return c;
}

static double complex_length2(Complex val) {
    return val.i * val.i + val.j * val.j;
}

static Complex complex_square(Complex val) {
    Complex r;
    r.i = val.i * val.i - val.j * val.j;
    r.j = 2.0 * val.i * val.j;
    return r;
}

static RGB palette[MAX_ITERATIONS + 1];

int main(void) {
    for (int i = 0; i <= MAX_ITERATIONS; i++)
        palette[i] = create_palette((double)i / MAX_ITERATIONS);

    double scale = 0.1 / SIZE;
    int checksum = 0;

    for (int y = 0; y < HEIGHT; y++) {
        for (int x = 0; x < WIDTH; x++) {
            int iteration = 0;
            Complex offset = { x * scale - 2.5, y * scale - 1 };
            Complex val = { 0.0, 0.0 };

            while (complex_length2(val) < MAX_RAD && iteration < MAX_ITERATIONS) {
                Complex sq = complex_square(val);
                val.i = sq.i + offset.i;
                val.j = sq.j + offset.j;
                iteration++;
            }

            RGB color = palette[iteration];
            checksum = checksum + color.r + color.g + color.b;
        }
    }
    printf("Checksum: %d\n", checksum);
    return 0;
}
