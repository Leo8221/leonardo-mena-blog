from __future__ import annotations

import random
import sys
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont


if len(sys.argv) != 2:
    raise SystemExit("Uso: python build-videojuegos-inflacion.py <directorio_salida>")

OUT = Path(sys.argv[1]).resolve()
OUT.mkdir(parents=True, exist_ok=True)
ICON_DIR = OUT / "icons"
ICON_DIR.mkdir(parents=True, exist_ok=True)

S = 4
W, H = 1800, 900
FONT = Path("C:/Windows/Fonts/segoeui.ttf")
FONT_BOLD = Path("C:/Windows/Fonts/segoeuib.ttf")


def f(size: int, bold: bool = False) -> ImageFont.FreeTypeFont:
    return ImageFont.truetype(str(FONT_BOLD if bold else FONT), size * S)


def q(value: float) -> int:
    return int(round(value * S))


def bb(values: tuple[float, float, float, float]) -> tuple[int, int, int, int]:
    return tuple(q(v) for v in values)  # type: ignore[return-value]


def rect(draw: ImageDraw.ImageDraw, values: tuple[float, float, float, float], **kwargs: object) -> None:
    draw.rectangle(bb(values), **kwargs)


def rounded(draw: ImageDraw.ImageDraw, values: tuple[float, float, float, float], radius: float, **kwargs: object) -> None:
    draw.rounded_rectangle(bb(values), radius=q(radius), **kwargs)


def ellipse(draw: ImageDraw.ImageDraw, values: tuple[float, float, float, float], **kwargs: object) -> None:
    draw.ellipse(bb(values), **kwargs)


def polygon(draw: ImageDraw.ImageDraw, points: list[tuple[float, float]], **kwargs: object) -> None:
    draw.polygon([(q(x), q(y)) for x, y in points], **kwargs)


def line(draw: ImageDraw.ImageDraw, points: list[tuple[float, float]], **kwargs: object) -> None:
    kwargs["width"] = q(float(kwargs.get("width", 1)))
    draw.line([(q(x), q(y)) for x, y in points], **kwargs)


def text(draw: ImageDraw.ImageDraw, pos: tuple[float, float], value: str, color: str, size: int, bold: bool = False, anchor: str = "la") -> None:
    draw.text((q(pos[0]), q(pos[1])), value, fill=color, font=f(size, bold), anchor=anchor)


def badge(draw: ImageDraw.ImageDraw, accent: str) -> None:
    ellipse(draw, (18, 18, 238, 238), fill="#08111d", outline="#040a11", width=4)
    ellipse(draw, (27, 27, 229, 229), fill="#17283b", outline=accent, width=5)
    ellipse(draw, (42, 42, 214, 214), outline="#5e7080", width=2)


def make_icon(kind: str, accent: str) -> Image.Image:
    image = Image.new("RGBA", (256 * S, 256 * S), (0, 0, 0, 0))
    draw = ImageDraw.Draw(image)
    badge(draw, accent)
    dark, cream, gold = "#0b1523", "#f1dfb6", "#e0bd62"
    blue, orange, red = "#77b1c0", "#d58a66", "#b7614d"

    if kind == "plex":
        for y, color in ((161, "#b7754d"), (145, "#d49a54"), (129, gold)):
            ellipse(draw, (61, y - 16, 170, y + 16), fill=color, outline=dark, width=4)
            rect(draw, (61, y - 2, 170, y + 12), fill=color, outline=None)
        text(draw, (190, 102), "PLEX", cream, 16, True, "ma")
        line(draw, [(191, 116), (216, 116)], fill=blue, width=3)

    elif kind == "isk":
        ellipse(draw, (72, 79, 183, 190), fill="#d79e4d", outline=dark, width=5)
        ellipse(draw, (87, 94, 168, 175), fill="#f0cd72", outline="#825e2d", width=3)
        text(draw, (128, 137), "ISK", "#8a5b28", 22, True, "mm")
        line(draw, [(128, 54), (128, 205)], fill=blue, width=3)
        polygon(draw, [(128, 51), (117, 65), (139, 65)], fill=blue, outline=dark, width=2)
        polygon(draw, [(128, 208), (117, 194), (139, 194)], fill=blue, outline=dark, width=2)

    elif kind == "fleet":
        polygon(draw, [(47, 154), (93, 122), (179, 123), (211, 154), (174, 168), (79, 168)], fill=blue, outline=dark, width=4)
        polygon(draw, [(87, 123), (108, 82), (161, 82), (178, 123)], fill="#b3d2d2", outline=dark, width=4)
        polygon(draw, [(109, 82), (135, 55), (161, 82)], fill=orange, outline=dark, width=4)
        line(draw, [(72, 180), (187, 180)], fill=gold, width=4)
        line(draw, [(102, 195), (167, 195)], fill="#e1edf0", width=2)

    elif kind == "prices":
        line(draw, [(57, 190), (57, 75)], fill=cream, width=3)
        line(draw, [(57, 190), (204, 190)], fill=cream, width=3)
        line(draw, [(69, 167), (104, 149), (129, 158), (162, 108), (201, 83)], fill=gold, width=7)
        polygon(draw, [(190, 78), (214, 84), (199, 103)], fill=orange, outline=dark, width=3)
        for x, y in ((104, 149), (129, 158), (162, 108)):
            ellipse(draw, (x - 7, y - 7, x + 7, y + 7), fill=blue, outline=dark, width=2)

    elif kind == "supply_down":
        for y, color in ((162, "#a8684b"), (144, "#cc8a4e"), (126, gold)):
            ellipse(draw, (55, y - 15, 135, y + 15), fill=color, outline=dark, width=3)
            rect(draw, (55, y - 2, 135, y + 10), fill=color, outline=None)
        line(draw, [(176, 78), (176, 181)], fill=blue, width=7)
        polygon(draw, [(176, 200), (157, 175), (195, 175)], fill=blue, outline=dark, width=3)

    elif kind == "tech_up":
        for x, height, color in ((62, 42, blue), (103, 72, gold), (144, 105, orange)):
            rect(draw, (x, 184 - height, x + 28, 184), fill=color, outline=dark, width=3)
        line(draw, [(55, 190), (195, 70)], fill=cream, width=7)
        polygon(draw, [(190, 61), (215, 72), (197, 91)], fill=orange, outline=dark, width=3)

    elif kind == "split":
        line(draw, [(128, 62), (128, 190)], fill=cream, width=4)
        line(draw, [(128, 112), (74, 82)], fill=gold, width=5)
        line(draw, [(128, 112), (184, 82)], fill=blue, width=5)
        ellipse(draw, (55, 68, 93, 103), fill=gold, outline=dark, width=3)
        ellipse(draw, (165, 68, 203, 103), fill=blue, outline=dark, width=3)
        text(draw, (128, 216), "≠", cream, 24, True, "ma")

    return image.resize((256, 256), Image.Resampling.LANCZOS)


def paste_icon(canvas: Image.Image, icon: Image.Image, x: int, y: int, size: int) -> None:
    scaled = icon.resize((q(size), q(size)), Image.Resampling.LANCZOS)
    canvas.alpha_composite(scaled, (q(x - size / 2), q(y - size / 2)))


def build_graph() -> Image.Image:
    canvas = Image.new("RGBA", (W * S, H * S), "#101a24")
    draw = ImageDraw.Draw(canvas)
    for y in range(H * S):
        t = y / (H * S)
        color = tuple(int(a * (1 - t) + b * t) for a, b in zip((22, 39, 54), (8, 14, 24)))
        draw.line((0, y, W * S, y), fill=(*color, 255))

    random.seed(27)
    for _ in range(48):
        x, y = random.randint(42, W - 42), random.randint(48, H - 50)
        r = random.choice((1, 2, 2, 3))
        ellipse(draw, (x - r, y - r, x + r, y + r), fill="#bdc9c4")

    text(draw, (72, 36), "NEW EDEN / MARKET CONTROL", "#8eb8c8", 15, True)
    text(draw, (72, 70), "QUARTERLY ECONOMIC NEWSLETTER", "#e2bd6a", 26, True)
    text(draw, (585, 70), "· Q4 2010", "#d6e1e3", 21)
    text(draw, (1728, 42), "CCP · PRICE LEVELS", "#d5e7ed", 16, True, "ra")
    line(draw, [(72, 111), (1728, 111)], fill="#4b6570", width=1)

    events = [
        (72, "OCT", "PLEX +6%", "demanda sostenida", "#6f9eaa", "plex"),
        (625, "NOV", "combustible y rigs ↑", "insumos más escasos", "#d2a95e", "tech_up"),
        (1178, "14 DIC", "regresan veteranos", "más demanda de bienes", "#d58b67", "fleet"),
    ]
    icon_map = {name: make_icon(name, color) for name, color in (
        ("plex", "#d2a95e"), ("isk", "#d2a95e"), ("fleet", "#d58b67"), ("prices", "#8eb8c8"),
        ("supply_down", "#d2a95e"), ("tech_up", "#d58b67"), ("split", "#8eb8c8"),
    )}
    for x, date, title, detail, color, icon in events:
        rounded(draw, (x, 140, x + 500, 222), 5, fill="#172733", outline=color, width=2)
        paste_icon(canvas, icon_map[icon], x + 46, 181, 62)
        text(draw, (x + 85, 155), date, "#a9c3cb", 13, True)
        text(draw, (x + 85, 178), title, "#f0d08a", 16, True)
        text(draw, (x + 85, 202), detail, "#b9c5c8", 13)

    modules = [
        (72, "OFERTA / FAUCET", "PLEX +18%", "creación diaria · nov → dic", "#d2a95e", "plex", "más oferta disponible"),
        (625, "DEMANDA", "más ISK gastado", "jugadores que vuelven", "#d58b67", "fleet", "reconstruyen su flota"),
        (1178, "ÍNDICE DE PRECIOS", "CPI +4,7%", "inflación general · trimestre", "#8eb8c8", "prices", "bienes y servicios"),
    ]
    centers = [322, 875, 1428]
    for i, (x, label, value, detail, color, icon, sub) in enumerate(modules):
        rounded(draw, (x, 280, x + 500, 592), 7, fill="#142330", outline=color, width=2)
        paste_icon(canvas, icon_map[icon], x + 82, 363, 116)
        text(draw, (x + 158, 326), label, color, 14, True)
        text(draw, (x + 158, 369), value, "#f3e7c7", 25, True)
        text(draw, (x + 158, 410), detail, "#b9c5c8", 14)
        line(draw, [(x + 36, 476), (x + 464, 476)], fill="#405a67", width=1)
        text(draw, (x + 36, 510), "LECTURA DE MERCADO", "#91b9c9", 11, True)
        text(draw, (x + 36, 542), sub, "#d7e0e8", 16)
        if i < 2:
            x1 = x + 500
            x2 = modules[i + 1][0]
            line(draw, [(x1 + 20, 436), (x2 - 20, 436)], fill="#e2bd6a", width=3)
            polygon(draw, [(x2 - 20, 436), (x2 - 39, 426), (x2 - 39, 446)], fill="#e2bd6a", outline="#0b1523", width=2)

    results = [
        (72, "PLEX −3,4%", "más oferta de PLEX", "supply_down", "#d2a95e"),
        (625, "TECH II +4,6%", "demanda de naves y módulos", "tech_up", "#d58b67"),
        (1178, "NO TODO SUBE", "la oferta también importa", "split", "#8eb8c8"),
    ]
    for x, title, detail, icon, color in results:
        rounded(draw, (x, 625, x + 500, 742), 5, fill="#111f2b", outline=color, width=2)
        paste_icon(canvas, icon_map[icon], x + 58, 684, 82)
        text(draw, (x + 115, 658), title, "#f0d08a", 16, True)
        text(draw, (x + 115, 695), detail, "#b9c5c8", 14)

    rounded(draw, (72, 775, 1728, 820), 4, fill="#1b2a33", outline="#4c6873", width=1)
    text(draw, (900, 797), "En el mismo universo: más PLEX puede bajar su precio, mientras el gasto sobre bienes empuja el CPI hacia arriba.", "#d5e7ed", 15, True, "mm")
    line(draw, [(72, 848), (1728, 848)], fill="#3f5661", width=1)
    text(draw, (72, 872), "Fuente: CCP · Quarterly Economic Newsletter, Q4 2010 · PLEX = Pilot License Extension", "#8fa2aa", 12)
    return canvas.resize((W, H), Image.Resampling.LANCZOS)


def main() -> None:
    icon_specs = {"plex": "#d2a95e", "isk": "#d2a95e", "fleet": "#d58b67", "prices": "#8eb8c8", "supply_down": "#d2a95e", "tech_up": "#d58b67", "split": "#8eb8c8"}
    for name, color in icon_specs.items():
        make_icon(name, color).save(ICON_DIR / f"{name}.png")
    build_graph().save(OUT / "eve-inflacion.png")


if __name__ == "__main__":
    main()
