import assert from 'node:assert/strict';
import fs from 'node:fs';
import vm from 'node:vm';
import test from 'node:test';

const context = vm.createContext({});
vm.runInContext(fs.readFileSync('atlas/js/chart-tooltips.js', 'utf8'), context);
const { findPointAt, findBoxAt } = context;

test('el margen táctil amplía el punto sin seleccionar puntos fuera del radio', () => {
  const point = { x: 10, y: 10, radius: 4 };
  assert.equal(findPointAt([point], { x: 16, y: 10 }, 2), point);
  assert.equal(findPointAt([point], { x: 17, y: 10 }, 2), undefined);
  assert.equal(findPointAt([point], { x: 17, y: 10 }, 14), point);
  assert.equal(findPointAt([], { x: 10, y: 10 }, 14), undefined);
});

test('barras y eventos incluyen sus bordes y respetan el orden cuando se solapan', () => {
  const first = { x: 4, y: 5, width: 10, height: 20 };
  const second = { ...first };
  assert.equal(findBoxAt([first, second], { x: 14, y: 25 }), first);
  assert.equal(findBoxAt([first], { x: 15, y: 25 }), undefined);
  assert.equal(findBoxAt([first], { x: 4, y: 4 }), undefined);
});
