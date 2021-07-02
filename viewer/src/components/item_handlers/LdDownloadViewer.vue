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
  <div class="container-vh-centering">
    <a :class="$style.content" :download="itemFileName" :href="itemDownloadUrl">
      <!-- TODO: show thumbnail instead of this generic file download icon? -->
      <fa-icon :class="$style.icon" icon="file-download" size="6x" />
      <div>{{ $t("download.download-file-fmt", [itemFileName]) }}</div>
    </a>
  </div>
</template>

<script lang="ts">
import { OtherItem } from "@/@types/gallery";
import Navigation from "@/services/navigation";
import { Component, Prop, Vue } from "vue-property-decorator";

@Component
export default class LdDownloadViewer extends Vue {
  @Prop({ required: true }) readonly item!: OtherItem;

  get itemFileName(): string {
    return Navigation.getFileName(this.item);
  }

  get itemDownloadUrl(): string {
    return this.$galleryStore.resourceRoot + this.item.properties.resource;
  }
}
</script>

<style lang="scss" module>
.content {
  text-align: center;
}

.icon {
  margin-bottom: 0.15em;
}
</style>
