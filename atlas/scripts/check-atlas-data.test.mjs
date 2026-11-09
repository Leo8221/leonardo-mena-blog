import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';

function atlas() {
  const family = { innerHTML: '', value: '' };
  const context = vm.createContext({
    document: {
      querySelector: () => null,
      getElementById: id => id === 'atlas-family' ? family : null
    },
    window: { matchMedia: () => ({ matches: false }) }
  });
  for (const file of ['utils', '../app', 'app-data', 'app-navigation', 'app-ui']) {
    const source = fs.readFileSync(`atlas/js/${file}.js`, 'utf8');
    // Cargamos las funciones sin arrancar la interfaz ni hacer peticiones.
    vm.runInContext(source.replace(/^initAtlasTheme\(\);$|^boot\(\);$/gm, ''), context);
  }
  const run = code => vm.runInContext(code, context);
  run(`state.data = ${fs.readFileSync('atlas/data/atlas-data.json', 'utf8')}`);
  return { run, family };
}

test('buscar conserva acentos, fuentes, familias y el orden de los módulos activos', () => {
  const { run } = atlas();
  run(`state.data.modules = [
    {id:'a', status:' Actívo ', family:'Macro', title:'Inflación', sourceInfo:{detail:'Banco Central'}},
    {id:'b', status:'activo', family:'Territorio', question:'Crédito', source:'Encuesta'},
    {id:'c', status:'activo', family:'Macro', visible:false, title:'Inflación'},
    {id:'d', status:'borrador', family:'Macro', title:'Inflación'}
  ]`);
  for (const [query, family, expected] of [
    ['', 'all', ['a', 'b']], ['inflacion', 'all', ['a']],
    ['CRÉDITO', 'all', ['b']], ['banco central', 'all', ['a']],
    ['encuesta', ' Macro ', []], ['', ' macro ', ['a']], ['inexistente', 'all', []]
  ]) {
    run(`state.query=${JSON.stringify(query)}; state.family=${JSON.stringify(family)}`);
    assert.deepEqual(Array.from(run('filteredModules().map(m => m.id)')), expected);
  }
  // Cambiar los datos debe reflejarse en la siguiente búsqueda, sin una caché obsoleta.
  run(`state.query=''; state.family='all'; state.data.modules[2].visible=true`);
  assert.deepEqual(Array.from(run('filteredModules().map(m => m.id)')), ['a', 'b', 'c']);
});

test('un filtro no cierra el módulo abierto y las familias ausentes vuelven a todas', () => {
  const { run, family } = atlas();
  run(`state.active='pulso-macro'; state.query='no existe'; syncFilterState()`);
  assert.equal(run('state.active'), 'pulso-macro');
  assert.equal(run('filteredModules().length'), 0);
  assert.doesNotMatch(family.innerHTML, /Sin datos/);
  run(`state.family='Sin datos'; syncFilterState()`);
  assert.equal(run('state.family'), 'all');
  assert.equal(family.value, 'all');
  assert.match(family.innerHTML, /Todos los temas/);
});

test('el formato numérico conserva redondeo, valores especiales y entradas convertibles', () => {
  const { run } = atlas();
  const expected = new Intl.NumberFormat('es-DO', { maximumFractionDigits: 1 });
  for (const expression of ['0', '-0', '1234.56', '-1234.56', '0.05', 'NaN', 'Infinity', 'null', 'undefined', '"12.34"', '12345678901234567890n']) {
    assert.equal(run(`formatNumber(${expression})`), expected.format(vm.runInNewContext(expression)));
  }
});

test('CSV conserva comillas, saltos, listas y valores ausentes', () => {
  const { run } = atlas();
  for (const [value, expected] of [[null, ''], [0, '0'], ['a,b', '"a,b"'], ['a"b', '"a""b"'], ['a\nb', '"a\nb"'], [['Norte', 'Sur'], 'Norte; Sur']]) {
    assert.equal(run(`csvEscape(${JSON.stringify(value)})`), expected);
  }
});
