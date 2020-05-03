<!--
-- ldgallery - A static generator which turns a collection of tagged
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
-->

<template>
  <div :class="$style.container">
    <a :class="$style.content" :download="itemFileName" :href="itemDownloadUrl">
      <!-- TODO: show thumbnail instead of this generic file download icon? -->
      <fa-icon :class="$style.icon" icon="file-download" size="6x" />
      <div>{{ $t("download.download-file-fmt", [itemFileName]) }}</div>
    </a>
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

@Component export default class LdDownload extends Vue {
  @Prop({ required: true }) readonly item!: Gallery.Item;

  get itemResource(): string {
    return (this.item.properties as Gallery.OtherProperties).resource;
  }

  get itemFileName(): string {
    const timeStamped = this.itemResource.split("/").pop() ?? "";
    return timeStamped.split("?")[0];
  }

  get itemDownloadUrl(): string {
    return this.$galleryStore.resourceRoot + this.itemResource;
  }
}
</script>

<style lang="scss" module>
.container {
  min-height: 100%;
  display: flex;
  align-items: center;
  justify-content: center;
}

.content {
  text-align: center;
}

.icon {
  margin-bottom: 0.15em;
}
</style>
