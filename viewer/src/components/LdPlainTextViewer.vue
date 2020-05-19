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
  <!-- Outer div necessary for the resize handle to appear on Firefox. -->
  <div :class="$style.content" class="fill">
    <!-- Using an iframe here to let the browser deal with content encoding detection. -->
    <iframe class="fill" :src="itemResourceUrl()" />
  </div>
</template>

<script lang="ts">
import { Component, Prop, Vue } from "vue-property-decorator";

@Component export default class LdPlainTextViewer extends Vue {
  @Prop({ required: true }) readonly plainTextItem!: Gallery.PlainText;

  itemResourceUrl(): string {
    return this.$galleryStore.resourceRoot + this.plainTextItem.properties.resource;
  }
}
</script>

<style lang="scss" module>
.content {
  margin: auto; // Old-school horizontal centering.

  // Allow the user to adjust the width of the text view for easier column reading.
  resize: horizontal;
  max-width: 100%; // Forbid overflow when resizing.
  overflow: hidden; // Necessary for the resize handle to be shown in Chromium.
  padding: .5em; // Necessary for the resize handle to be selectable on qutebrowser.

  // Re-normalise page colour.
  background-color: white;
}
</style>
