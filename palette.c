/*
 *  Generate 216 colors for our little XPM palette.
 *  Too lazy to do this in Haskell.
 * 
 *  To compile: gcc -Wall -std=c99 palette.c
 */

#include <stdint.h>
#include <stdio.h>

typedef struct rgbquad { 
        uint8_t blue;
        uint8_t green;
        uint8_t red;
        uint8_t reserved;
} rgbquad_t;

uint32_t cast (rgbquad_t q)
{
        return q.red << 16 | q.green << 8 | q.blue;
}
        
int main (int argc, char **argv)
{
        uint32_t deltas[]       = {0x00, 0x33, 0x66, 0x99, 0xcc, 0xff};
        uint32_t palette[6*6*6] = {0};

        unsigned index = 0;
        
        for (unsigned red = 0; red < 6; red++) {
                for (unsigned green = 0; green < 6; green++) {
                        for (unsigned blue = 0; blue < 6; blue++) {
                                /* rgbquad  */
                                rgbquad_t q;
                                q.blue     = deltas[blue];
                                q.green    = deltas[green];
                                q.red      = deltas[red];                                
                                q.reserved = 0;
                                
                                palette[index] = cast(q);
                                index++;
                        }
                }
        }

        for (unsigned i = 0; i < index; i += 6) {
                printf ("%06x, %06x, %06x, %06x, %06x, %06x,\n",
                        palette[i],   palette[i+1], palette[i+2],
                        palette[i+3], palette[i+4], palette[i+5]);
        }

        return 0;
}

