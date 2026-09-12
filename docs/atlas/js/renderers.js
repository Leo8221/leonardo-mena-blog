// Atlas chart renderers. Keep these pure enough to reuse from fullscreen views.
function renderBarRows(rows, options) {
  const max = options.max || Math.max(...rows.map((item) => Math.abs(item[options.valueField])), 1);
  return rows.map((item) => {
    const value = Number(item[options.valueField]);
    const width = Math.min(100, Math.abs(value) / max * 100);
    return `
      <div class="bar-row">
        <strong>${escapeHtml(item[options.labelField])}</strong>
        <span class="bar-track"><span class="bar-fill" style="width:${width}%"></span></span>
        <span>${formatNumber(value)}${options.suffix || ""}</span>
      </div>
    `;
  }).join("");
}

function renderContributionRows(rows, options) {
  const sortedRows = rows
    .slice()
    .sort((a, b) => Math.abs(Number(b[options.valueField])) - Math.abs(Number(a[options.valueField])));
  const max = Math.max(...sortedRows.map((item) => Math.abs(Number(item[options.valueField]))), 1);
  return sortedRows.map((item, index) => {
    const value = Number(item[options.valueField]);
    const width = Math.min(100, Math.abs(value) / max * 100);
    const sign = value > 0 ? "+" : "";
    return `
      <div class="contribution-row" style="--share:${width}%">
        <span class="contribution-rank">${index + 1}</span>
        <strong>${escapeHtml(item[options.labelField])}</strong>
        <span>${sign}${formatNumber(value)}${options.suffix || ""}</span>
      </div>
    `;
  }).join("");
}
