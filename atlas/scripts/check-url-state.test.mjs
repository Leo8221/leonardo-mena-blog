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
    history: {
      replaceState: (_state, _title, next) => { context.window.location.href = String(next); },
      pushState: (_state, _title, next) => { context.window.location.href = String(next); }
    }
  });
  for (const file of ['atlas/js/utils.js', 'atlas/app.js', 'atlas/js/app-data.js', 'atlas/js/app-panels.js']) {
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

test('los módulos publicados conservan sus notas de lectura predeterminadas', () => {
  const app = atlas();
  for (const module of app.run('visibleModules()')) {
    const html = app.run(`renderModuleGuide(findModule(${JSON.stringify(module.id)}))`);
    assert.match(html, /<summary>Notas<\/summary>/, module.id);
    assert.match(html, /<dt>Límite<\/dt>/, module.id);
  }
});

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

test('cada selector conserva la métrica al compartir y volver a abrir la vista', () => {
  const app = atlas();
  const views = {
    'pulso-macro': ['dolar', 'inflacion', 'imae', 'tpm'],
    'comercio-exterior': ['exports', 'imports'],
    'mercado-laboral': ['employment', 'informality', 'wageIndex'],
    'territorio-infraestructura': ['business_density', 'opportunity'],
    'laboratorio-visual': ['business', 'mipymes', 'tourism']
  };
  for (const [view, metrics] of Object.entries(views)) {
    for (const metric of metrics) {
      app.navigate(`?view=${view}&metric=${metric}`);
      app.run('applyStateFromUrl()');
      assert.equal(app.run('activeMetricForModule(findModule(state.active))'), metric);
      const shared = app.run('buildAtlasUrl().search');
      app.navigate(shared);
      app.run('applyStateFromUrl()');
      assert.equal(app.run('activeMetricForModule(findModule(state.active))'), metric);
    }
  }
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
  app.run('syncFilterState = syncSearchState = renderNavigation = renderStage = () => {};');
  app.run('resetAtlasFilters()');
  assert.equal(app.url().searchParams.has('q'), false);
  assert.equal(app.url().searchParams.has('filter'), false);
  assert.equal(app.url().searchParams.get('view'), 'pulso-macro');
});

test('abrir un indicador elige su métrica y conserva la búsqueda de regreso', () => {
  const app = atlas('https://example.test/atlas/?q=precios');
  app.run('applyStateFromUrl(); syncSearchState = renderNavigation = renderStage = trackAtlasEvent = () => {};');
  app.run('setActive("pulso-macro", {metric: "inflacion"})');
  assert.equal(app.url().searchParams.get('metric'), 'inflacion');
  assert.equal(app.url().searchParams.get('q'), 'precios');
  assert.equal(app.run('state.focusStage'), true);
  app.run('setActive("overview")');
  assert.equal(app.url().searchParams.has('view'), false);
  assert.equal(app.url().searchParams.has('metric'), false);
  assert.equal(app.url().searchParams.get('q'), 'precios');
});
