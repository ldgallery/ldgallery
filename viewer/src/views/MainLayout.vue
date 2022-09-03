<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2022  Guillaume FOUET
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
-->

<template>
  <div :class="{ [$style.fullscreen]: uiStore.fullscreen, [$style.fullwidth]: uiStore.fullWidth }">
    <LdLoading v-if="isLoading" />
    <SplashScreen
      v-else-if="uiStore.splashScreenEnabled"
      @validation="validateSpashScreen"
    />
    <template v-else-if="galleryStore.config && galleryStore.galleryIndex">
      <LayoutTop :class="[$style.layout, $style.layoutTop]" />
      <LayoutLeft :class="[$style.layout, $style.layoutLeft]" />
      <router-view
        ref="content"
        :class="[$style.layout, $style.layoutContent]"
        class="scrollbar"
        tabindex="2"
      />
    </template>
  </div>
</template>

<script setup lang="ts">
import LdLoading from '@/components/LdLoading.vue';
import { useLdKeepFocus } from '@/services/ui/ldKeepFocus';
import { useLdSaveScroll } from '@/services/ui/ldSaveScroll';
import { useGalleryStore } from '@/store/galleryStore';
import { useUiStore } from '@/store/uiStore';
import { VueInstance } from '@vueuse/core';
import { createToast } from 'mosha-vue-toastify';
import { nextTick, ref } from 'vue';
import LayoutLeft from './layout/left/LayoutLeft.vue';
import LayoutTop from './layout/top/LayoutTop.vue';
import SplashScreen from './SplashScreen.vue';

const uiStore = useUiStore();
const galleryStore = useGalleryStore();

const content = ref<VueInstance>();
const isLoading = ref(true);

useLdSaveScroll(content);
const { moveFocus } = useLdKeepFocus(content);

history.replaceState({ ldgallery: 'ENTRYPOINT' }, '');
fetchGalleryItems();

function fetchGalleryItems() {
  isLoading.value = true;
  galleryStore
    .fetchConfig()
    .then(uiStore.initFromConfig)
    .then(galleryStore.fetchGalleryItems)
    .then(moveFocus)
    .finally(() => (isLoading.value = false))
    .catch(displayError);
}

function displayError(reason: unknown) {
  createToast(String(reason), {
    type: 'danger',
    position: 'top-center',
    timeout: 10000,
    showIcon: true,
    onClose: () => !isLoading.value && fetchGalleryItems(),
  });
}

function validateSpashScreen() {
  uiStore.validateSpashScreen();
  nextTick(moveFocus);
}
</script>

<style lang="scss" module>
@import "~@/assets/scss/theme";

:root {
  --layout-top: #{$layout-top};
  --layout-left: #{$layout-left};
}
:global(body),
:global(html) {
  font-family: $family-sans-serif;
  height: 100%;
  overflow: hidden;
  touch-action: none;
  background-color: $content-bgcolor;
  margin: 0;
}
.layout {
  position: fixed;
  top: 0;
  bottom: 0;
  left: 0;
  right: 0;
  &.layoutTop {
    height: $layout-top;
    z-index: 1;
  }
  &.layoutLeft {
    top: $layout-top;
    width: $layout-left;
    z-index: 2;
  }
  &.layoutContent {
    top: var(--layout-top);
    left: var(--layout-left);
    z-index: 3;
    overflow-x: hidden;
    &:focus {
      outline: none;
    }
  }
}
.fullscreen {
  --layout-top: 0px;
  @extend .fullwidth;
}
.fullwidth {
  --layout-left: 0px;
  .layout {
    &.layout-left {
      transform: translate(-$layout-left, 0);
    }
  }
}

.layout {
  &.layoutTop {
    background-color: $panel-top-bgcolor;
    color: $panel-top-txtcolor;
  }
  &.layoutLeft {
    background-color: $panel-left-bgcolor;
    color: $panel-left-txtcolor;
  }
  &.layoutContent {
    background-color: $content-bgcolor;
  }
}
</style>
