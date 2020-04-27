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
import "hammerjs";

/**
 * Mousewheel and pinch zoom handler.
 */
export default class LdZoom {
  readonly containerElement: HTMLDivElement;
  readonly imageElement: HTMLImageElement;
  readonly pictureProperties: Gallery.PictureProperties;
  readonly maxScaleFactor: number;
  readonly scrollZoomSpeed: number;
  scaleFactor: number = 0.0;

  constructor(
    containerElement: HTMLDivElement, imageElement: HTMLImageElement,
    pictureProperties: Gallery.PictureProperties,
    maxScaleFactor: number, scrollZoomSpeed: number
  ) {
    this.containerElement = containerElement;
    this.imageElement = imageElement;
    this.pictureProperties = pictureProperties;
    this.maxScaleFactor = maxScaleFactor;
    this.scrollZoomSpeed = scrollZoomSpeed;
  }

  /**
   * Register event listeners.
   */
  public install() {
    this.updateImageScale(this.scaleFactor);

    new ResizeObserver(() => {
      this.updateImageScale(this.scaleFactor);
    }).observe(this.containerElement);

    this.containerElement.addEventListener('wheel', wheelEvent => {
      wheelEvent.preventDefault();
      const scaleFactor = this.scaleFactor - Math.sign(wheelEvent.deltaY) * this.scrollZoomSpeed;
      this.zoom(wheelEvent.offsetX, wheelEvent.offsetY, scaleFactor);
    });

    const pinchListener = new Hammer(this.containerElement);
    pinchListener.get('pinch').set({enable: true});
    this.installPinchHandler(pinchListener);
  }

  private installPinchHandler(pinchListener: HammerManager) {
    let startScaleFactor = 0.0;

    pinchListener.on('pinchstart', (pinchEvent: HammerInput) => {
      startScaleFactor = this.scaleFactor;
    });

    pinchListener.on('pinchmove', (pinchEvent: HammerInput) => {
      const focusX = pinchEvent.center.x + this.containerElement.scrollLeft;
      const focusY = pinchEvent.center.y + this.containerElement.scrollTop;
      const scaleFactor = pinchEvent.scale * startScaleFactor;
      this.zoom(focusX, focusY, scaleFactor);
    });
  }

  /**
   * Returns the picture resolution as it should be displayed.
   */
  private getDisplayResolution(): Gallery.Resolution {
    return {
      width: this.pictureProperties.resolution.width * this.scaleFactor,
      height: this.pictureProperties.resolution.height * this.scaleFactor,
    };
  }

  /**
   * Applies scaling to the DOM image element.
   * To call after internal intermediate computations because DOM properties aren't stable.
   */
  private resizeImageElement() {
    const imageDim = this.getDisplayResolution();
    this.imageElement.width = imageDim.width;
    this.imageElement.height = imageDim.height;
  }

  /**
   * Centers the image element inside its container if it fits, or stick to the top and left borders otherwise.
   * It's depressingly hard to do in pure CSS…
   */
  private recenterImageElement() {
    const imageDim = this.getDisplayResolution();
    const marginLeft = Math.max((this.containerElement.clientWidth - imageDim.width) / 2, 0);
    const marginTop = Math.max((this.containerElement.clientHeight - imageDim.height) / 2, 0);
    this.imageElement.style.marginLeft = `${marginLeft}px`;
    this.imageElement.style.marginTop = `${marginTop}px`;
  }

  private zoom(focusX: number, focusY: number, scaleFactor: number) {
    const imageDim = this.getDisplayResolution();
    const ratioX = focusX / imageDim.width;
    const ratioY = focusY / imageDim.height;
    this.updateImageScale(Math.min(scaleFactor, this.maxScaleFactor));

    const newImageDim = this.getDisplayResolution();
    this.containerElement.scrollLeft -= focusX - ratioX * newImageDim.width;
    this.containerElement.scrollTop -= focusY - ratioY * newImageDim.height;
  }

  private updateImageScale(newScaleFactor: number) {
    const horizontalFillRatio = this.containerElement.clientWidth / this.pictureProperties.resolution.width;
    const verticalFillRatio = this.containerElement.clientHeight / this.pictureProperties.resolution.height;
    const minScaleFactor = Math.min(horizontalFillRatio, verticalFillRatio, 1.0);
    this.scaleFactor = Math.max(newScaleFactor, minScaleFactor);
    this.resizeImageElement();
    this.recenterImageElement();
  }
}
