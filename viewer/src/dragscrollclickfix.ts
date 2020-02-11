/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--
-- This program is free software: you can redistribute it and/or modify
-- it under the terms of the GNU Affero General Public License as
-- published by the Free Software Foundation, either version 3 of the
-- License, or (at your option) any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU Affero General Public License for more details.
--
-- You should have received a copy of the GNU Affero General Public License
-- along with this program.  If not, see <https://www.gnu.org/licenses/>.
*/

// https://github.com/donmbelembe/vue-dragscroll/issues/61
export default class DragScrollClickFix {

  readonly DRAG_DELAY = 250; // This is the minimal delay to consider a click to be a drag, mostly usefull for touch devices

  timer: NodeJS.Timeout | null = null;
  dragging: boolean = false;

  onDragScrollStart() {
    this.timer = setTimeout(() => this.onTimer(), this.DRAG_DELAY);
  }

  onTimer() {
    this.timer = null;
    this.dragging = true;
  }

  onDragScrollEnd() {
    if (this.timer) {
      clearTimeout(this.timer);
      this.timer = null;
    }
    setTimeout(() => this.dragging = false);
  }

  onClickCapture(e: MouseEvent) {
    if (this.dragging) {
      this.dragging = false;
      e.preventDefault();
      e.stopPropagation();
    }
  }
}