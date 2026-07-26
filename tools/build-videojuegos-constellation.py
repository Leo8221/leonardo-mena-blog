from __future__ import annotations

import random
import sys
from pathlib import Path

from PIL import Image, ImageDraw, ImageFont, ImageFilter


if len(sys.argv) != 2:
    raise SystemExit("Uso: python build-videojuegos-constellation.py <directorio_salida>")

OUT = Path(sys.argv[1]).resolve()
OUT.mkdir(parents=True, exist_ok=True)
ASSET_DIR = OUT / "icons"
ASSET_DIR.mkdir(parents=True, exist_ok=True)

S = 4
ICON_SIZE = 256
W, H = 1800, 975

FONT_REG = Path("C:/Windows/Fonts/segoeui.ttf")
FONT_BOLD = Path("C:/Windows/Fonts/segoeuib.ttf")


def font(size: int, bold: bool = False) -> ImageFont.FreeTypeFont:
    return ImageFont.truetype(str(FONT_BOLD if bold else FONT_REG), size * S)


def xy(value: float) -> int:
    return int(round(value * S))


def box(values: tuple[float, float, float, float]) -> tuple[int, int, int, int]:
    return tuple(xy(v) for v in values)  # type: ignore[return-value]


def polygon(draw: ImageDraw.ImageDraw, points: list[tuple[float, float]], **kwargs: object) -> None:
    draw.polygon([(xy(x), xy(y)) for x, y in points], **kwargs)


def line(draw: ImageDraw.ImageDraw, points: list[tuple[float, float]], **kwargs: object) -> None:
    kwargs["width"] = xy(float(kwargs.get("width", 1)))
    draw.line([(xy(x), xy(y)) for x, y in points], **kwargs)


def ellipse(draw: ImageDraw.ImageDraw, values: tuple[float, float, float, float], **kwargs: object) -> None:
    draw.ellipse(box(values), **kwargs)


def rect(draw: ImageDraw.ImageDraw, values: tuple[float, float, float, float], **kwargs: object) -> None:
    draw.rectangle(box(values), **kwargs)


def draw_badge(draw: ImageDraw.ImageDraw, accent: str) -> None:
    ellipse(draw, (18, 24, 238, 244), fill="#07101d", outline="#050b13", width=4)
    ellipse(draw, (24, 18, 232, 226), fill="#14233a", outline=accent, width=5)
    ellipse(draw, (39, 33, 217, 211), outline="#657187", width=2)
    line(draw, [(49, 68), (207, 68)], fill="#ffffff", width=1)
    line(draw, [(56, 205), (200, 205)], fill="#000000", width=3)


def make_icon(kind: str, accent: str) -> Image.Image:
    image = Image.new("RGBA", (ICON_SIZE * S, ICON_SIZE * S), (0, 0, 0, 0))
    draw = ImageDraw.Draw(image)
    draw_badge(draw, accent)

    dark = "#10192a"
    cream = "#f1ddb1"
    gold = "#e3bd62"
    blue = "#75aebe"
    terracotta = "#d88462"
    pale = "#f2e5c4"

    if kind == "study":
        polygon(draw, [(54, 114), (119, 102), (119, 178), (53, 166)], fill=gold, outline=dark, width=3)
        polygon(draw, [(137, 102), (202, 114), (203, 166), (137, 178)], fill="#f0ce76", outline=dark, width=3)
        line(draw, [(128, 103), (128, 178)], fill="#76552b", width=4)
        line(draw, [(70, 128), (103, 124)], fill="#f8e8b2", width=2)
        line(draw, [(154, 124), (187, 128)], fill="#f8e8b2", width=2)
        polygon(draw, [(183, 76), (201, 82), (164, 146), (147, 139)], fill=terracotta, outline=dark, width=2)
        polygon(draw, [(199, 81), (211, 85), (203, 96)], fill=gold, outline=dark, width=2)

    elif kind == "work":
        rect(draw, (56, 105, 200, 172), fill=blue, outline=dark, width=4)
        line(draw, [(56, 130), (200, 130)], fill=gold, width=4)
        line(draw, [(92, 104), (99, 83), (157, 83), (164, 104)], fill=cream, width=7)
        rect(draw, (119, 127, 137, 145), fill=gold, outline=dark, width=2)
        line(draw, [(71, 157), (185, 157)], fill="#c3e0e2", width=2)

    elif kind == "business":
        rect(draw, (57, 116, 199, 184), fill=pale, outline=dark, width=4)
        rect(draw, (52, 95, 204, 124), fill=terracotta, outline=dark, width=4)
        for x in (72, 105, 138, 171):
            rect(draw, (x, 98, x + 16, 124), fill="#f1cb78", outline=None)
        polygon(draw, [(49, 96), (128, 54), (207, 96)], fill="#b8634e", outline=dark, width=4)
        rect(draw, (111, 143, 145, 184), fill=terracotta, outline=dark, width=3)
        rect(draw, (72, 140, 96, 163), fill=blue, outline=dark, width=3)
        rect(draw, (160, 140, 184, 163), fill=blue, outline=dark, width=3)

    elif kind == "diploma":
        rect(draw, (64, 86, 192, 166), fill=cream, outline=dark, width=4)
        rect(draw, (75, 98, 181, 114), fill="#d6a957", outline=dark, width=2)
        line(draw, [(91, 132), (165, 132)], fill="#9d7c42", width=3)
        line(draw, [(91, 148), (149, 148)], fill="#9d7c42", width=3)
        ellipse(draw, (100, 145, 154, 199), fill=terracotta, outline=dark, width=3)
        polygon(draw, [(112, 184), (128, 173), (144, 184), (140, 211), (128, 200), (116, 211)], fill=gold, outline=dark, width=2)

    elif kind == "experience":
        polygon(draw, [(48, 137), (86, 103), (121, 126), (143, 105), (207, 140), (180, 181), (132, 155), (103, 182)], fill="#6f9eaa", outline=dark, width=4)
        line(draw, [(89, 132), (123, 151), (143, 132)], fill=cream, width=8)
        line(draw, [(108, 116), (128, 136), (149, 116)], fill=gold, width=4)
        ellipse(draw, (105, 111, 125, 131), fill=cream, outline=dark, width=2)
        ellipse(draw, (139, 111, 159, 131), fill=cream, outline=dark, width=2)

    elif kind == "invest":
        for y, fill in ((167, "#b87549"), (151, "#d3944c"), (135, gold)):
            ellipse(draw, (65, y - 13, 150, y + 13), fill=fill, outline=dark, width=3)
            rect(draw, (65, y - 1, 150, y + 10), fill=fill, outline=None)
        line(draw, [(160, 180), (160, 91), (192, 91)], fill=cream, width=7)
        polygon(draw, [(190, 76), (215, 91), (190, 106)], fill=terracotta, outline=dark, width=3)

    elif kind == "specialize":
        ellipse(draw, (61, 61, 195, 195), outline=gold, width=7)
        ellipse(draw, (88, 88, 168, 168), fill="#1f3550", outline=cream, width=3)
        line(draw, [(128, 69), (128, 187)], fill=terracotta, width=4)
        line(draw, [(69, 128), (187, 128)], fill=terracotta, width=4)
        polygon(draw, [(128, 93), (141, 119), (170, 122), (148, 141), (154, 170), (128, 155), (102, 170), (108, 141), (86, 122), (115, 119)], fill=gold, outline=dark, width=3)

    elif kind == "promote":
        for x, height, fill in ((65, 38, "#6f9eaa"), (103, 68, blue), (141, 98, gold)):
            rect(draw, (x, 180 - height, x + 29, 180), fill=fill, outline=dark, width=3)
        line(draw, [(59, 184), (183, 68)], fill=cream, width=7)
        polygon(draw, [(177, 58), (202, 69), (184, 86)], fill=terracotta, outline=dark, width=3)

    elif kind == "scale":
        rect(draw, (78, 83, 178, 174), fill=pale, outline=dark, width=4)
        polygon(draw, [(70, 84), (128, 51), (186, 84)], fill=terracotta, outline=dark, width=4)
        rect(draw, (105, 130, 151, 174), fill="#6f9eaa", outline=dark, width=3)
        rect(draw, (88, 98, 105, 115), fill=blue, outline=dark, width=2)
        rect(draw, (151, 98, 168, 115), fill=blue, outline=dark, width=2)
        line(draw, [(58, 197), (77, 181), (93, 188), (112, 164)], fill=gold, width=6)
        polygon(draw, [(106, 157), (126, 160), (113, 177)], fill=gold, outline=dark, width=2)

    elif kind == "resource":
        polygon(draw, [(78, 76), (178, 76), (145, 120), (111, 120)], fill=cream, outline=dark, width=4)
        polygon(draw, [(111, 137), (145, 137), (178, 181), (78, 181)], fill=cream, outline=dark, width=4)
        polygon(draw, [(111, 120), (145, 120), (137, 143), (119, 143)], fill=gold, outline=dark, width=2)
        line(draw, [(83, 69), (173, 69)], fill=terracotta, width=7)
        line(draw, [(83, 188), (173, 188)], fill=terracotta, width=7)

    else:
        ellipse(draw, (82, 82, 174, 174), fill=gold, outline=dark, width=3)

    return image.resize((ICON_SIZE, ICON_SIZE), Image.Resampling.LANCZOS)


def paste_icon(canvas: Image.Image, icon: Image.Image, x: int, y: int, size: int) -> None:
    scaled_size = xy(size)
    canvas.alpha_composite(icon.resize((scaled_size, scaled_size), Image.Resampling.LANCZOS), (xy(x - size / 2), xy(y - size / 2)))


def text(draw: ImageDraw.ImageDraw, xy_pos: tuple[int, int], value: str, fill: str, size: int, bold: bool = False, anchor: str = "la") -> None:
    draw.text(xy_pos, value, font=font(size, bold), fill=fill, anchor=anchor)


def build_graph() -> Image.Image:
    canvas = Image.new("RGBA", (W * S, H * S), "#101a35")
    draw = ImageDraw.Draw(canvas)
    for y in range(H * S):
        t = y / (H * S)
        color = tuple(int(a * (1 - t) + b * t) for a, b in zip((28, 49, 84), (8, 14, 29)))
        draw.line((0, y, W * S, y), fill=(*color, 255))

    random.seed(15)
    for _ in range(54):
        x, y = random.randint(45, W - 45), random.randint(55, H - 55)
        r = random.choice((2, 2, 3, 4))
        ellipse(draw, (x - r, y - r, x + r, y + r), fill="#e9e8d4")

    text(draw, (72 * S, 32 * S), "SELECCIONA TU RUTA", "#e8d49b", 25, True)
    text(draw, (72 * S, 69 * S), "Una ventaja desbloqueada · una alternativa que dejas atrás", "#a7b5ca", 16)
    text(draw, (1725 * S, 38 * S), "NIVEL 01", "#a7b5ca", 16, anchor="ra")

    accent = {"study": "#dfbd62", "work": "#8fb8c9", "business": "#d58b67"}
    icons = {name: make_icon(name, color) for name, color in (
        ("study", accent["study"]), ("work", accent["work"]), ("business", accent["business"]),
        ("diploma", accent["study"]), ("experience", accent["work"]), ("invest", accent["business"]),
        ("specialize", accent["study"]), ("promote", accent["work"]), ("scale", accent["business"]),
        ("resource", "#e2bd63"),
    )}

    center_x, center_y = 900, 190
    text(draw, (center_x * S, 86 * S), "RECURSOS ESCASOS", "#f3e7c7", 22, True, "ma")
    text(draw, (center_x * S, 113 * S), "tiempo · dinero · atención", "#aebed0", 16, anchor="ma")
    paste_icon(canvas, make_icon("resource", "#e2bd63"), center_x, center_y, 118)

    columns = [
        (210, "study", "ESTUDIAR", "capital humano", "renuncia: ingreso presente", "diploma", "specialize", "TÍTULO", "ESPECIALIZAR", "más opciones laborales", "salario potencial"),
        (900, "work", "TRABAJAR", "ingreso y experiencia", "renuncia: tiempo de formación", "experience", "promote", "EXPERIENCIA", "ASCENSO", "red laboral", "estabilidad futura"),
        (1590, "business", "EMPRENDER", "autonomía y control", "renuncia: estabilidad", "invest", "scale", "INVERTIR", "ESCALAR", "control del proyecto", "riesgo y retorno"),
    ]

    for x, kind, title, benefit, cost, second_icon, third_icon, second_label, third_label, second_sub, third_sub in columns:
        left, right = x - 225, x + 225
        rect(draw, (left, 260, right, 820), fill="#0d162b", outline=accent[kind], width=3)
        # Conectores se mantienen detrás de los tokens.
        line(draw, [(center_x, center_y + 59), (x - 90, 325 - 58)], fill=accent[kind], width=4)
        line(draw, [(x - 90, 325 + 58), (x - 90, 495 - 47)], fill=accent[kind], width=4)
        line(draw, [(x - 90, 495 + 47), (x - 90, 650 - 47)], fill=accent[kind], width=4)

        paste_icon(canvas, icons[kind], x - 90, 325, 112)
        paste_icon(canvas, icons[second_icon], x - 90, 495, 92)
        paste_icon(canvas, icons[third_icon], x - 90, 650, 92)

        text(draw, ((x - 15) * S, 306 * S), title, "#f3e7c7", 20, True)
        text(draw, ((x - 15) * S, 343 * S), benefit, "#aebed0", 15)
        text(draw, ((x - 15) * S, 478 * S), second_label, "#f3e7c7", 18, True)
        text(draw, ((x - 15) * S, 515 * S), second_sub, "#aebed0", 14)
        text(draw, ((x - 15) * S, 633 * S), third_label, "#f3e7c7", 18, True)
        text(draw, ((x - 15) * S, 670 * S), third_sub, "#aebed0", 14)

        rect(draw, (left + 40, 742, right - 40, 785), fill="#17233b", outline=accent[kind], width=2)
        text(draw, (x * S, 763 * S), f"BENEFICIO · {benefit}", "#d7e0e8", 14, True, "mm")
        text(draw, (x * S, 850 * S), cost, accent[kind], 14, anchor="ma")

    text(draw, (900 * S, 925 * S), "Cada ruta ofrece una ventaja; ninguna elimina el coste de elegir.", "#d7e0e8", 16, anchor="mm")
    return canvas.resize((W, H), Image.Resampling.LANCZOS)


def main() -> None:
    icon_specs = {
        "study": "#dfbd62", "work": "#8fb8c9", "business": "#d58b67",
        "diploma": "#dfbd62", "experience": "#8fb8c9", "invest": "#d58b67",
        "specialize": "#dfbd62", "promote": "#8fb8c9", "scale": "#d58b67", "resource": "#e2bd63",
    }
    for name, color in icon_specs.items():
        make_icon(name, color).save(ASSET_DIR / f"{name}.png")
    build_graph().save(OUT / "constellation-decisions.png")


if __name__ == "__main__":
    main()
