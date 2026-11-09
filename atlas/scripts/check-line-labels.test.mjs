import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';

test('las fechas caben en el gráfico móvil sin perder observaciones', () => {
  const rows = JSON.parse(fs.readFileSync('atlas/data/atlas-data.json', 'utf8')).series.macro;
  for (const width of [250, 320, 768, 1184]) {
    const labels = [];
    let pointCount = 0;
    const context2d = {
      beginPath() {}, moveTo() {}, lineTo() {}, setLineDash() {}, stroke() {}, fill() {},
      arc() { pointCount++; },
      measureText: text => ({ width: text.length * 7 }),
      fillText(text, x, y) { if (y === 306) labels.push({ text, x, end: x + text.length * 7 }); }
    };
    const context = vm.createContext({
      setupCanvas: () => context2d,
      clearCanvas() {}, drawGrid() {}, drawCanvasTitle() {}, drawLegend() {}, bindCanvasTooltip() {},
      CHART_SYSTEM: { colors: { blue: '#466a8f', muted: '#555' } },
      formatNumber: String,
      canvas: { getBoundingClientRect: () => ({ width, height: 320 }) },
      rows
    });
    vm.runInContext(fs.readFileSync('atlas/js/chart-series.js', 'utf8'), context);
    vm.runInContext('drawLineChart(canvas, rows.map(r => r.period), rows.map(r => r.dolar), "Tipo de cambio")', context);
    assert.equal(pointCount, rows.length, 'se conservan todos los puntos');
    assert.equal(labels[0].text, rows[0].period);
    assert.equal(labels.at(-1).text, rows.at(-1).period);
    for (let i = 0; i < labels.length; i++) {
      assert.ok(labels[i].x >= 0 && labels[i].end <= width, 'fecha dentro del canvas');
      if (i) assert.ok(labels[i].x >= labels[i - 1].end, 'fechas sin solaparse');
    }
  }
});
