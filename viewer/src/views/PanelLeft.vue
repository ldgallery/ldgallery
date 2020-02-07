<!-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2019-2020  Guillaume FOUET
--               2020       Pacien TRAN-GIRARD
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
  <div class="flex-column">
    <ld-tag-input />
    <ld-proposition class="scrollbar no-scroll-x" />

    <!--
      TODO: just remove the following "mode" switch: manually toggling it leads to inconsistent views.
      The two modes should instead be made clear by the use of the breadcrumbs: the path would not appear anymore during searches.
      Entering any query already switches automatically into search mode.
      It is already possible to exit the search mode by clearing the filters, either by using the home, back and up navigation buttons, or manually by removing the tags in the input fieds.
    -->
    <ld-mode-radio style="opacity: 0.5; position: absolute; bottom: 0; z-index: -1;" />

  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop } from "vue-property-decorator";

@Component
export default class PanelLeft extends Vue {}
</script>

<style lang="scss">
</style>
