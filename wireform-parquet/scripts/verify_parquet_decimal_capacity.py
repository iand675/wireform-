#!/usr/bin/env python3
"""Prove the bounded DECIMAL-capacity arithmetic in Parquet.Decimal.

Only exact integer/Fraction arithmetic is used. The logarithm bounds come
from convergent atanh series with geometric tail bounds. Farey neighbors
then rule out every floor ambiguity for the full signed32-bit width domain.
No runtime decimal precision or byte-width limit is substituted for the spec.
"""
from dataclasses import dataclass
from fractions import Fraction
import json
from pathlib import Path
import re


@dataclass(frozen=True)
class Bounds:
    lower: Fraction
    upper: Fraction


def logarithm_bounds(x, terms):
    total = Fraction(0)
    for k in range(terms):
        total += 2 * x ** (2 * k + 1) / (2 * k + 1)
    tail = 2 * x ** (2 * terms + 1) / ((2 * terms + 1) * (1 - x * x))
    return Bounds(total, total + tail)


def verify():
    source = (Path(__file__).resolve().parents[1] / "src/Parquet/Decimal.hs").read_text()
    assert re.search(r"decimalPrecisionFits\s*::\s*Int32\s*->\s*Int32\s*->\s*Bool", source)
    numerator = int(re.search(r"log10TwoLowerNumerator\s*=\s*(\d+)", source).group(1))
    denominator = int(re.search(r"log10TwoDenominator\s*=\s*(\d+)", source).group(1))
    ln2 = logarithm_bounds(Fraction(1, 3), 100)
    ln_five_fourths = logarithm_bounds(Fraction(1, 9), 100)
    exact_lower = ln2.lower / (3 * ln2.lower + ln_five_fourths.upper)
    exact_upper = ln2.upper / (3 * ln2.upper + ln_five_fourths.lower)
    lower = Fraction(numerator, denominator)
    upper = Fraction(numerator + 1, denominator)
    assert lower < exact_lower < exact_upper < upper
    left = Fraction(5107137094, 16965542197)
    right = Fraction(579001193, 1923400330)
    maximum_bits = 8 * ((1 << 31) - 1) - 1
    assert left < lower < upper < right
    assert right.numerator * left.denominator - left.numerator * right.denominator == 1
    assert left.denominator + right.denominator > maximum_bits
    # Every interior k/n has n >= b+d. Thus no integer boundary can
    # separate n*lower from n*upper in the entire admitted width domain.
    for width in range(1, 4097):
        bits = 8 * width - 1
        actual = bits * numerator // denominator
        # Independent exact powers, bounded to 4KiB integers in this check.
        assert 10 ** actual <= 2 ** bits < 10 ** (actual + 1)
    return {"log_bounds_proved": True, "farey_floor_certificate_proved": True,
            "full_width_domain_max": (1 << 31) - 1, "maximum_bits": maximum_bits,
            "small_width_exact_power_checks": 4096,
            "minimum_interior_denominator": left.denominator + right.denominator}


if __name__ == "__main__":
    print(json.dumps(verify(), indent=2))
