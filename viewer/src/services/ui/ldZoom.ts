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

import { PictureProperties, Resolution } from '@/@types/gallery';
import { importHammer } from '@/plugins/asyncLib';
import { CSSProperties, Ref } from 'vue';

/**
 * Mousewheel and pinch zoom handler.
 */
export const useLdZoom = (
  imageStyle: Ref<CSSProperties>,
  containerElement: HTMLDivElement,
  imageElement: HTMLImageElement,
  pictureProperties: PictureProperties,
  maxScaleFactor = 10,
  scrollZoomSpeed: number = 1 / 7,
) => {
  let scaleFactor = 0.0;

  /**
   * Register event listeners.
   */
  updateImageScale(scaleFactor);

  new ResizeObserver(() => {
    updateImageScale(scaleFactor);
  }).observe(containerElement);

  containerElement.addEventListener('wheel', wheelEvent => {
    wheelEvent.preventDefault();
    const newScaleFactor = scaleFactor - Math.sign(wheelEvent.deltaY) * (scrollZoomSpeed * scaleFactor);
    zoom(wheelEvent.offsetX, wheelEvent.offsetY, newScaleFactor);
  });

  importHammer().then(() => {
    const pinchListener = new Hammer(containerElement);
    pinchListener.get('pinch').set({ enable: true });
    installPinchHandler(pinchListener);
  });

  return { imageStyle };

  // ---

  function installPinchHandler(pinchListener: HammerManager) {
    let startScaleFactor = 0.0;

    pinchListener.on('pinchstart', () => {
      startScaleFactor = scaleFactor;
    });

    pinchListener.on('pinchmove', (pinchEvent: HammerInput) => {
      const focusX = pinchEvent.center.x + containerElement.scrollLeft;
      const focusY = pinchEvent.center.y + containerElement.scrollTop;
      const scaleFactor = pinchEvent.scale * startScaleFactor;
      zoom(focusX, focusY, scaleFactor);
    });
  }

  /**
   * Returns the picture resolution as it should be displayed.
   */
  function getDisplayResolution(): Resolution {
    return {
      width: pictureProperties.resolution.width * scaleFactor,
      height: pictureProperties.resolution.height * scaleFactor,
    };
  }

  /**
   * Applies scaling to the DOM image element.
   * To call after internal intermediate computations because DOM properties aren't stable.
   */
  function resizeImageElement() {
    const imageDim = getDisplayResolution();
    imageElement.width = imageDim.width;
    imageElement.height = imageDim.height;
  }

  /**
   * Centers the image element inside its container if it fits, or stick to the top and left borders otherwise.
   * It's depressingly hard to do in pure CSS…
   */
  function recenterImageElement() {
    const imageDim = getDisplayResolution();
    const marginLeft = Math.max((containerElement.clientWidth - imageDim.width) / 2, 0);
    const marginTop = Math.max((containerElement.clientHeight - imageDim.height) / 2, 0);
    imageStyle.value.marginLeft = `${marginLeft}px`;
    imageStyle.value.marginTop = `${marginTop}px`;
  }

  function zoom(focusX: number, focusY: number, scaleFactor: number) {
    const imageDim = getDisplayResolution();
    const ratioX = focusX / imageDim.width;
    const ratioY = focusY / imageDim.height;
    updateImageScale(Math.min(scaleFactor, maxScaleFactor));

    const newImageDim = getDisplayResolution();
    containerElement.scrollLeft -= focusX - ratioX * newImageDim.width;
    containerElement.scrollTop -= focusY - ratioY * newImageDim.height;
  }

  function updateImageScale(newScaleFactor: number) {
    const horizontalFillRatio = containerElement.clientWidth / pictureProperties.resolution.width;
    const verticalFillRatio = containerElement.clientHeight / pictureProperties.resolution.height;
    const minScaleFactor = Math.min(horizontalFillRatio, verticalFillRatio, 1.0);
    scaleFactor = Math.max(newScaleFactor, minScaleFactor);
    resizeImageElement();
    recenterImageElement();
  }
};
