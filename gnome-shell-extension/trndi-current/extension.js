/* exported init enable disable */

const Main = imports.ui.main;
const { St, GLib } = imports.gi;
const Mainloop = imports.mainloop;

const UUID = 'trndi-current@slicke.com';

let _indicator = null;
let _timeoutId = null;

function _cachePath() {
  const cacheDir = GLib.get_user_cache_dir();
  return GLib.build_filenamev([cacheDir, 'trndi', 'current.txt']);
}

function _readCurrentValue() {
  const path = _cachePath();

  try {
    const [ok, bytes] = GLib.file_get_contents(path);
    if (!ok)
      return null;

    const text = bytes.toString();
    const trimmed = text.trim();
    if (trimmed.length === 0)
      return null;

    return trimmed;
  } catch (e) {
    return null;
  }
}

function _tick() {
  if (!_indicator)
    return GLib.SOURCE_REMOVE;

  const v = _readCurrentValue();
  _indicator.set_text(v ?? '--');

  return GLib.SOURCE_CONTINUE;
}

function init() {
  // no-op
}

function enable() {
  if (_indicator)
    return;

  _indicator = new St.Label({
    text: '--',
    y_align: 2,
    style_class: 'panel-status-menu-box'
  });

  Main.panel._rightBox.insert_child_at_index(_indicator, 0);

  // Poll every 5 seconds; file is written by Trndi on updates.
  _timeoutId = Mainloop.timeout_add_seconds(5, _tick);
  _tick();
}

function disable() {
  if (_timeoutId) {
    Mainloop.source_remove(_timeoutId);
    _timeoutId = null;
  }

  if (_indicator) {
    _indicator.destroy();
    _indicator = null;
  }
}
