#define PROLOG_MODULE "math"
#include <SWI-Prolog.h>
#include <math.h>

PREDICATE(pi, 1)
{ A1 = M_PI;
}