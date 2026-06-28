function bindPointTooltip(canvas, points, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const hitPadding = touchLikeEvent(event) ? CHART_SYSTEM.hitPadding.touch : CHART_SYSTEM.hitPadding.mouse;
    const found = points.find((point) => {
      const dx = cursor.x - point.x;
      const dy = cursor.y - point.y;
      return Math.sqrt(dx * dx + dy * dy) <= point.radius + hitPadding;
    });

    return found ? content(found) : null;
  });
}

function bindBoxTooltip(canvas, boxes, content) {
  bindCanvasTooltip(canvas, (event) => {
    const cursor = getCanvasPoint(canvas, event);
    const found = boxes.find((box) => (
      cursor.x >= box.x &&
      cursor.x <= box.x + box.width &&
      cursor.y >= box.y &&
      cursor.y <= box.y + box.height
    ));

    return found ? content(found) : null;
  });
}

function bindCanvasTooltip(canvas, resolveContent) {
  canvas.onmousemove = null;
  canvas.onclick = null;
  canvas.onmouseleave = null;
  canvas.onpointermove = (event) => {
    if (event.pointerType === "touch") return;
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip();
      return;
    }
    showTooltip(html, event, { pinned: false });
  };

  canvas.onpointerdown = (event) => {
    const html = resolveContent(event);
    canvas.style.cursor = html ? "pointer" : "default";
    if (!html) {
      hideTooltip(true);
      return;
    }
    showTooltip(html, event, { pinned: event.pointerType !== "mouse" });
    if (event.pointerType !== "mouse") event.preventDefault();
  };

  canvas.onpointerleave = () => {
    canvas.style.cursor = "default";
    hideTooltip();
  };

  canvas.onpointercancel = () => {
    canvas.style.cursor = "default";
    hideTooltip(true);
  };
}
