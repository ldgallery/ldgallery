/* ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2020  Pacien TRAN-GIRARD
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

// polyfill still required for IE and Safari, see https://caniuse.com/#feat=resizeobserver
import ResizeObserver from 'resize-observer-polyfill';

/**
 * Mousewheel picture zoom helper.
 */
export default class LdZoom {
  readonly containerElement: HTMLDivElement;
  readonly imageElement: HTMLImageElement;
  readonly maxScaleFactor: number;
  readonly zoomSpeed: number;
  scaleFactor: number = 0.0;

  constructor(
    containerElement: HTMLDivElement, imageElement: HTMLImageElement,
    maxScaleFactor: number, zoomSpeed: number
  ) {
    this.containerElement = containerElement;
    this.imageElement = imageElement;
    this.maxScaleFactor = maxScaleFactor;
    this.zoomSpeed = zoomSpeed;
  }

  /**
   * Register event listeners.
   * The dimension of the image should be known before calling this method.
   */
  public install() {
    new ResizeObserver(() => {
      this.setImageScale(this.scaleFactor);
      this.recenterImageElement();
    }).observe(this.containerElement);

    this.containerElement.addEventListener('wheel', wheelEvent => {
      wheelEvent.preventDefault();
      this.zoom(wheelEvent);
    });

    // TODO: handle pinch-to-zoom.
  }

  /**
   * Centers the image element inside its container if it fits, or stick to the top and left borders otherwise.
   * It's depressingly hard to do in pure CSS…
   */
  private recenterImageElement() {
    const marginLeft = Math.max((this.containerElement.clientWidth - this.imageElement.clientWidth) / 2, 0);
    const marginTop = Math.max((this.containerElement.clientHeight - this.imageElement.clientHeight) / 2, 0);
    this.imageElement.style.marginLeft = `${marginLeft}px`;
    this.imageElement.style.marginTop = `${marginTop}px`;
  }

  private zoom(wheelEvent: WheelEvent) {
    const ratioX = wheelEvent.offsetX / this.imageElement.clientWidth;
    const ratioY = wheelEvent.offsetY / this.imageElement.clientHeight;

    const zoomDelta = -Math.sign(wheelEvent.deltaY) * this.zoomSpeed;
    this.setImageScale(Math.min(this.scaleFactor + zoomDelta, this.maxScaleFactor));

    this.containerElement.scrollLeft -= wheelEvent.offsetX - ratioX * this.imageElement.clientWidth;
    this.containerElement.scrollTop -= wheelEvent.offsetY - ratioY * this.imageElement.clientHeight;
  }

  private setImageScale(newScaleFactor: number) {
    const horizontalFillRatio = this.containerElement.clientWidth / this.imageElement.naturalWidth;
    const verticalFillRatio = this.containerElement.clientHeight / this.imageElement.naturalHeight;
    const minScaleFactor = Math.min(horizontalFillRatio, verticalFillRatio, 1.0);
    this.scaleFactor = Math.max(newScaleFactor, minScaleFactor);

    this.imageElement.width = this.scaleFactor * this.imageElement.naturalWidth;
    this.imageElement.height = this.scaleFactor * this.imageElement.naturalHeight;
    this.recenterImageElement();
  }
}
