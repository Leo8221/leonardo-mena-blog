import path from "node:path";
import { spawn } from "node:child_process";
import { articlePath } from "./articles.mjs";

function openTarget(target) {
  spawn("powershell.exe", ["-NoProfile", "-Command", "Start-Process -FilePath $env:BLOG_OPEN_TARGET"],
    { env: { ...process.env, BLOG_OPEN_TARGET: target }, windowsHide: true, stdio: "ignore" });
}

export function openLocalFile(root, relative, directory = false) {
  const file = articlePath(root, relative);
  openTarget(directory ? path.dirname(file) : file);
}

export function openBrowser(url) { openTarget(url); }

export function previewPost(root, relative) {
  const file = articlePath(root, relative);
  const command = `$arguments = @('-NoProfile', '-NoExit', '-ExecutionPolicy', 'Bypass', '-File',
    ('"' + $env:BLOG_PREVIEW_SCRIPT + '"'), '-Article', ('"' + $env:BLOG_PREVIEW_ARTICLE + '"'));
    Start-Process -FilePath powershell.exe -ArgumentList $arguments`;
  spawn("powershell.exe", ["-NoProfile", "-Command", command], {
    env: { ...process.env, BLOG_PREVIEW_SCRIPT: path.join(root, "tools/preview-article.ps1"), BLOG_PREVIEW_ARTICLE: file },
    windowsHide: true, stdio: "ignore"
  });
}
