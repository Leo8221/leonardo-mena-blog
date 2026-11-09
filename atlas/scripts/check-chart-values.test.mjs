import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';

function chartContext(extra = {}) {
  const context = vm.createContext(extra);
  for (const file of ['utils.js', 'chart-series.js', 'app-panels.js']) {
    vm.runInContext(fs.readFileSync(`atlas/js/${file}`, 'utf8'), context);
  }
  return context;
}

test('la TPM conserva centésimas en etiquetas y tablas', () => {
  const context = chartContext();
  for (const value of [5.25, 5.5, 5.75, 6]) {
    const expected = new Intl.NumberFormat('es-DO', { minimumFractionDigits: 2, maximumFractionDigits: 2 }).format(value);
    assert.equal(vm.runInContext(`formatNumber(${value}, 2)`, context), expected);
    assert.equal(vm.runInContext(`formatTableCell(${value}, 2)`, context), expected);
  }
  assert.equal(vm.runInContext('formatTableCell(null, 2)', context), '—');
});

test('una tasa constante queda centrada y los ejes contienen todos los valores', () => {
  const context = chartContext();
  const constant = vm.runInContext('lineChartScale([5.25, 5.25], 0.25)', context);
  assert.equal((constant.min + constant.max) / 2, 5.25);
  assert.equal(constant.steps, 4);
  for (const values of [[5.25, 6], [-3, 2], [0, 0], [59.33, null, 63.64]]) {
    context.values = values;
    const scale = vm.runInContext('lineChartScale(values)', context);
    for (const value of values.filter(Number.isFinite)) {
      assert.ok(value >= scale.min && value <= scale.max);
    }
    assert.ok(scale.max > scale.min);
  }
});

test('los datos ausentes interrumpen la línea y no se convierten en ceros', () => {
  const moves = [], segments = [], points = [];
  const ctx = {
    beginPath() {}, moveTo(x, y) { moves.push([x, y]); },
    lineTo(x, y) { segments.push([x, y]); }, arc(x, y) { points.push([x, y]); },
    setLineDash() {}, stroke() {}, fill() {}, fillText() {},
    measureText: text => ({ width: text.length * 6 })
  };
  const context = chartContext({
    CHART_SYSTEM: { colors: {} }, canvas: { getBoundingClientRect: () => ({ width: 320, height: 320 }) }
  });
  Object.assign(context, {
    setupCanvas: () => ctx, clearCanvas() {}, drawGrid() {}, drawCanvasTitle() {}, drawLegend() {}, bindCanvasTooltip() {}
  });
  vm.runInContext('drawLineChart(canvas, ["A", "B", "C", "D"], [5.25, null, 5.5, 5.75], "TPM", {stepped:true, decimals:2})', context);
  assert.equal(points.length, 3);
  assert.equal(moves.length, 2, 'se reinicia después del dato ausente');
  assert.equal(segments.length, 2, 'solo se une C con D mediante un escalón');
});

test('la TPM publicada conserva su corte propio y coincide con la extracción', () => {
  const live = JSON.parse(fs.readFileSync('atlas/data/bcrd-live-data.json', 'utf8'));
  const published = JSON.parse(fs.readFileSync('atlas/data/atlas-data.json', 'utf8'));
  assert.deepEqual(published.series.policyRate, live.series.policyRate.map(({ period, tpm }) => ({ period, tpm })));
  assert.equal(published.liveDataCutoff.policyRate, published.series.policyRate.at(-1).period);
  assert.equal(published.series.policyRate.length, 24);
  const context = chartContext({ state: { data: published, macroMetric: 'tpm' } });
  vm.runInContext(fs.readFileSync('atlas/js/app-modules.js', 'utf8'), context);
  assert.equal(vm.runInContext('macroChartRows().at(-1).period', context), published.liveDataCutoff.policyRate);
  assert.ok(vm.runInContext('moduleDatasets({chart:"macro"}).some(d => d.id === "tpm" && d.rows.length === 24)', context));
});

test('la variación de precios conserva centésimas y se expresa en porcentaje', () => {
  const context = chartContext({ state: { data: { series: { prices: {
    components: [{ component: 'Alimentos', contribution: 0.14 }], passThrough: []
  } } } } });
  for (const file of ['renderers.js', 'app-modules.js']) {
    vm.runInContext(fs.readFileSync(`atlas/js/${file}`, 'utf8'), context);
  }
  const html = vm.runInContext('renderPrices()', context);
  assert.ok(html.includes('+0.14 %'));
  assert.ok(!html.includes('p.p.'));
});
