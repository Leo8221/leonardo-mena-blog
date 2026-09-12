import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';

function atlas(url = 'https://example.test/atlas/') {
  const search = { value: '', focus() {} };
  const context = vm.createContext({
    URL, URLSearchParams,
    window: { location: { href: url }, matchMedia: () => ({ matches: false }) },
    document: { querySelector: () => null, getElementById: id => id === 'atlas-search' ? search : null },
    history: { replaceState: (_state, _title, next) => { context.window.location.href = String(next); } }
  });
  for (const file of ['atlas/js/utils.js', 'atlas/app.js', 'atlas/js/app-data.js']) {
    vm.runInContext(fs.readFileSync(file, 'utf8'), context, { filename: file });
  }
  vm.runInContext(`state.data = ${fs.readFileSync('atlas/data/atlas-data.json', 'utf8')}`, context);
  return {
    run: code => vm.runInContext(code, context),
    navigate: next => { context.window.location.href = 'https://example.test/atlas/' + next; },
    search,
    url: () => new URL(context.window.location.href)
  };
}

test('volver a la portada borra vista, consulta, familia y métrica anteriores', () => {
  const app = atlas('https://example.test/atlas/?view=pulso-macro&metric=inflacion&q=precios&filter=Macro');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.macroMetric'), 'inflacion');
  app.navigate('');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.active'), 'overview');
  assert.equal(app.run('state.query'), '');
  assert.equal(app.run('state.family'), 'all');
  assert.equal(app.run('state.macroMetric'), 'dolar');
  assert.equal(app.search.value, '');
});

test('los enlaces antiguos por hash siguen abriendo el módulo', () => {
  const app = atlas('https://example.test/atlas/#pulso-macro');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.active'), 'pulso-macro');
});

test('métricas y mapas desconocidos conservan una vista válida', () => {
  const app = atlas('https://example.test/atlas/?view=laboratorio-visual&metric=desconocida&map=desconocido');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.visualMap'), 'business');
  app.navigate('?view=pulso-macro&metric=desconocida');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.macroMetric'), 'dolar');
});

test('volver a un mapa sin región elimina el filtro regional anterior', () => {
  const app = atlas();
  const region = app.run('state.data.series.territory.provinces[0].region');
  app.navigate('?view=territorio-infraestructura&region=' + encodeURIComponent(region));
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.territoryRegion'), region);
  app.navigate('?view=territorio-infraestructura');
  app.run('applyStateFromUrl()');
  assert.equal(app.run('state.territoryRegion'), 'all');
});

test('limpiar filtros también limpia la URL compartible', () => {
  const app = atlas('https://example.test/atlas/?view=pulso-macro&filter=Macro&q=precios');
  app.run('applyStateFromUrl()');
  app.run('syncFilterState = syncSearchState = renderNavigation = renderMobileNavigation = renderStage = () => {};');
  app.run('resetAtlasFilters()');
  assert.equal(app.url().searchParams.has('q'), false);
  assert.equal(app.url().searchParams.has('filter'), false);
  assert.equal(app.url().searchParams.get('view'), 'pulso-macro');
});
