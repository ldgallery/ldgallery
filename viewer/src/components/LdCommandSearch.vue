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
  <div class="flex">
    <b-button @click="clear">
      <fa-icon icon="eraser" />
      <span>{{$t('command.search.clear')}}</span>
    </b-button>
    <b-button expanded :loading="loading" @click="search">
      <fa-icon icon="search" />
      <span>{{$t('command.search.search')}}</span>
    </b-button>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Emit } from "vue-property-decorator";

@Component
export default class LdCommandSearch extends Vue {
  loading: boolean = false;

  @Emit()
  clear(e: HTMLButtonElement) {
    return e;
  }

  @Emit()
  search(e: HTMLButtonElement) {
    this.loading = true;
    this.$nextTick(() => (this.loading = false));
    return e;
  }
}
</script>

<style lang="scss">
</style>
