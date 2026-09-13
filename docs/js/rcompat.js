// Small helpers that mirror R's vector semantics (rep(), `:`, seq(), etc).
// The pattern generators in js/patterns/*.js are near-literal transliterations
// of the original shiny/R/*_module.R code, and rely on these helpers behaving
// exactly like their R namesakes (recycling rules, seq()'s "seq along a
// vector" gotcha, etc) rather than the "obviously intended" behaviour.

// R's `a:b` - counts down when a > b, always integer step of 1 or -1.
function seqR(from, to) {
  const out = [];
  if (from <= to) {
    for (let i = from; i <= to; i++) out.push(i);
  } else {
    for (let i = from; i >= to; i--) out.push(i);
  }
  return out;
}

// R's seq(from, to, length.out = n)
function seqLen(from, to, lengthOut) {
  if (lengthOut === 1) return [from];
  const step = (to - from) / (lengthOut - 1);
  const out = [];
  for (let i = 0; i < lengthOut; i++) out.push(from + step * i);
  return out;
}

// R's rep(x, times = , each = ). `times` and `each` are scalars here (the
// vector forms of `times` are not used anywhere in the ported code).
function repR(x, { times = 1, each = 1 } = {}) {
  let out = [];
  if (each !== 1) {
    for (const v of x) {
      for (let i = 0; i < each; i++) out.push(v);
    }
  } else {
    out = x.slice();
  }
  if (times !== 1) {
    const base = out.slice();
    out = [];
    for (let i = 0; i < times; i++) out = out.concat(base);
  }
  return out;
}

function revR(x) {
  return x.slice().reverse();
}

function meanR(x) {
  return x.reduce((a, b) => a + b, 0) / x.length;
}

function rangeR(x) {
  return [Math.min(...x), Math.max(...x)];
}

function diffRangeR(x) {
  const [lo, hi] = rangeR(x);
  return hi - lo;
}

// R's sum()/cumsum() accumulate in extended ("long double") precision on
// platforms that support it, which is more accurate than naive
// double-precision accumulation over many terms. Kahan summation closes most
// of that gap in plain JS doubles, which matters here because
// square.js sums/cumsums ~reps^2 width values before dividing back out - a
// plain running total can drift by enough ULPs to flip a round(x, 3) at a
// boundary a few steps downstream.
function sumR(x) {
  let sum = 0;
  let comp = 0;
  for (const v of x) {
    const y = v - comp;
    const t = sum + y;
    comp = t - sum - y;
    sum = t;
  }
  return sum;
}

function cumsumR(x) {
  const out = [];
  let sum = 0;
  let comp = 0;
  for (const v of x) {
    const y = v - comp;
    const t = sum + y;
    comp = t - sum - y;
    sum = t;
    out.push(sum);
  }
  return out;
}

// Since R 4.0.0, round() uses a more elaborate algorithm (extended-precision
// re-evaluation plus its own tie handling, see Martin Mächler's "Fixing
// round()") than naively scaling by 10^digits in double precision, which can
// itself introduce error at the scaling step (see git history for an example
// this broke). `toFixed()` correctly rounds from the exact binary value of
// the double it's given, which matches R's round() for the vast majority of
// values - the two can still disagree, by exactly one unit in the 3rd
// decimal place, on the rare value that lands within float noise of an exact
// tie, since R's algorithm can treat such a value as the tie it was clearly
// meant to be while toFixed() honours the literal (if noisy) bits it's
// given. That residual disagreement is accounted for with extra tolerance in
// the square pattern's parity tests rather than chased further here.
function roundR(x, digits = 0) {
  return parseFloat(x.toFixed(digits));
}

if (typeof module !== "undefined" && module.exports) {
  module.exports = {
    seqR,
    seqLen,
    repR,
    revR,
    sumR,
    meanR,
    rangeR,
    diffRangeR,
    cumsumR,
    roundR,
  };
}
