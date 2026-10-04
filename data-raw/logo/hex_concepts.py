"""Generate the MortalityLaws hex logo as SVG.

Concept: the cohort hugged by two mortality curves, on a pointy-top hex
(512 x 591, matching the existing brand shape).

  - ceiling = survivorship l(x)
  - floor   = Gompertz hazard mu(x)  (no infant mortality spike)
  Both start parallel at the left, converge and cross on the right, where
  the cohort is squeezed out. The people sit on a strict row/column grid;
  gaps open along the tail to suggest attrition.

Two colorways: hex-mortalitylaws-lime (lime field), hex-mortalitylaws-dark (ink field).

Run: python data-raw/logo/hex_concepts.py
Then rasterize render_hires.html (1024 px slots) to inst/figures/hex-mortalitylaws-*.png.
"""
import math

W, H = 512, 591
HEX = [(256, 0), (512, 147.8), (512, 443.4), (256, 591), (0, 443.4), (0, 147.8)]

INK_TOP, INK_BOT = "#14242F", "#0B141B"
INK = "#16242E"
LIME = "#C6E44B"
ICE = "#EAF2F5"

SANS = "Segoe UI Variable Display, Segoe UI, Helvetica Neue, Arial, sans-serif"

X0, X1 = 84, 432
Y0, Y1 = 165, 402
SPAN = Y1 - Y0
MARGIN = 13
ROWS = (178, 204, 230, 256, 282, 308, 334, 360, 386)


def X(x):
    return X0 + (X1 - X0) * x / 100.0


def surv(x):
    return 1.0 / (1.0 + math.exp((x - 72.0) / 8.0))


def gomp(x):
    return math.exp((x - 58.0) / 12.0) / math.exp(42.0 / 12.0)


def curve(fn, x0=X0, x1=X1, ybase=Y1, span=SPAN, n=201):
    pts = []
    for i in range(n):
        t = i / (n - 1)
        px = x0 + (x1 - x0) * t
        py = ybase - span * fn(100.0 * t)
        pts.append((px, py))
    return "M " + " L ".join(f"{px:.2f},{py:.2f}" for px, py in pts)


def wordmark(y, size=46, strong=ICE, light=LIME):
    return (
        f'<text x="256" y="{y}" text-anchor="middle" font-family="{SANS}" '
        f'font-size="{size}" letter-spacing="0.5" fill="{strong}">'
        f'<tspan font-weight="650">Mortality</tspan>'
        f'<tspan font-weight="300" fill="{light}">Laws</tspan></text>'
    )


def wrap(inner, bg_top, bg_bot, grad_id):
    hexpts = " ".join(f"{x},{y}" for x, y in HEX)
    return f"""<svg xmlns="http://www.w3.org/2000/svg" width="{W}" height="{H}" viewBox="0 0 {W} {H}">
  <defs>
    <linearGradient id="{grad_id}" x1="0" y1="0" x2="0.35" y2="1">
      <stop offset="0" stop-color="{bg_top}"/>
      <stop offset="1" stop-color="{bg_bot}"/>
    </linearGradient>
    <clipPath id="hexclip"><polygon points="{hexpts}"/></clipPath>
  </defs>
  <polygon points="{hexpts}" fill="url(#{grad_id})"/>
  <g clip-path="url(#hexclip)">
{inner}
  </g>
</svg>
"""


def rnd(i, j):
    v = math.sin(i * 127.1 + j * 311.7) * 43758.5453
    return v - math.floor(v)


def person(x, y, s=1.0, color=INK, op=1.0):
    return (
        f'<g transform="translate({x:.1f},{y:.1f}) scale({s})" stroke="{color}" '
        f'stroke-width="2.2" fill="none" stroke-linecap="round" opacity="{op}">'
        f'<circle cx="0" cy="-6.5" r="2.6" fill="{color}" stroke="none"/>'
        f'<line x1="0" y1="-3.2" x2="0" y2="4.5"/>'
        f'<line x1="-3.4" y1="-0.6" x2="3.4" y2="-0.6"/>'
        f'<line x1="-3" y1="8.4" x2="0" y2="4.5"/><line x1="3" y1="8.4" x2="0" y2="4.5"/></g>'
    )


def cohort(color, op=1.0, cols=25, rows=ROWS):
    out = []
    for i in range(cols):
        t = i / (cols - 1)
        x = X0 + (X1 - X0) * t
        age = 100.0 * t
        y_top = Y1 - SPAN * surv(age) + MARGIN
        y_bot = Y1 - SPAN * gomp(age) - MARGIN
        # attrition: individuals drop out along the tail, leaving gaps
        keep = 1.0 if age <= 48 else 1.0 - 0.85 * (age - 48.0) / 52.0
        for j, y in enumerate(rows):
            if not (y_top < y < y_bot):
                continue
            if rnd(i, j) > keep:
                continue
            out.append(person(x, y, 0.88, color, op))
    return out


def hybrid(bg_top, bg_bot, grad_id, crowd_color, crowd_op, surv_col, haz_col, strong, light):
    inner = "\n    ".join(cohort(crowd_color, crowd_op)) + f"""
    <path d="{curve(surv)}" fill="none" stroke="{surv_col}" stroke-width="9" stroke-linecap="round"/>
    <path d="{curve(gomp)}" fill="none" stroke="{haz_col}" stroke-width="13" stroke-linecap="round"/>
    {wordmark(492, strong=strong, light=light)}"""
    return wrap(inner, bg_top, bg_bot, grad_id)


def hex_lime():
    return hybrid(LIME, "#B4D43A", "lime", INK, 0.85, INK, INK, INK, INK)


def hex_dark():
    return hybrid(INK_TOP, INK_BOT, "ink", ICE, 0.55, ICE, LIME, ICE, LIME)


CONCEPTS = {
    "hex-mortalitylaws-lime": hex_lime,
    "hex-mortalitylaws-dark": hex_dark,
}

if __name__ == "__main__":
    for name, fn in CONCEPTS.items():
        with open(f"data-raw/logo/{name}.svg", "w", encoding="utf-8") as f:
            f.write(fn())
        print(f"wrote data-raw/logo/{name}.svg")
