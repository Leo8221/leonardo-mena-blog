import { createRequire } from "node:module";
import { resolve } from "node:path";

if (process.argv.length !== 5) {
  throw new Error("Uso: node render-svg-preview.mjs <svg_entrada> <png_salida> <node_modules>");
}

const [, , inputArg, outputArg, modulesArg] = process.argv;
const requireFromBundle = createRequire(resolve(modulesArg, "sharp", "package.json"));
const sharp = requireFromBundle("sharp");

await sharp(resolve(inputArg), { density: 180 })
  .resize({ width: 1600, withoutEnlargement: false })
  .png()
  .toFile(resolve(outputArg));

console.log(`SVG_PREVIEW_OK: ${resolve(outputArg)}`);
