<!-- ldgallery - A static generator which turns a collection of tagged
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
-->

<template>
  <div v-if="isReady">
    <b-steps
      v-model="activeStep"
      class="pathBreadcrumb"
      type="is-info"
      :has-navigation="false"
      :animated="false"
      @input="onStepClick"
    >
      <b-step-item
        v-for="item in $galleryStore.currentItemPath"
        :key="item.path"
        :label="item.title"
        :icon="getIcon(item)"
        :to="item.path"
      />
    </b-steps>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import Gallery from "./Gallery.vue";

@Component
export default class PanelTop extends Vue {
  activeStep: number = -1;

  mounted() {
    this.currentItemPathChanged();
  }

  get isReady() {
    return this.activeStep >= 0;
  }

  @Watch("$galleryStore.currentItemPath")
  currentItemPathChanged() {
    this.activeStep = -1;
    this.$nextTick(() => (this.activeStep = this.$galleryStore.currentItemPath.length - 1));
  }

  getIcon(item: Gallery.Item) {
    switch (item.properties.type) {
      case "picture":
        return "image";
      case "directory":
        return "folder";
    }
  }

  onStepClick(index: number) {
    const item = this.$galleryStore.currentItemPath[index];
    if (item) this.$router.push(item.path);
  }
}
</script>

<style lang="scss">
.pathBreadcrumb {
  margin: 3px;
}
.step-title {
  color: white;
}
</style>
