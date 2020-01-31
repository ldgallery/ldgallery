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
  <div>
    <gallery-search v-if="$uiStore.isModeSearch" :items="currentSearch" />
    <gallery-directory v-else-if="checkType('directory')" :directory="$galleryStore.currentItem" />
    <gallery-picture v-else-if="checkType('picture')" :picture="$galleryStore.currentItem" />
    <div v-else>{{$t("gallery.unknowntype")}}</div>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import { Operation } from "@/@types/tag/Operation";
import Tools from "@/tools";
import GallerySearch from "./GallerySearch.vue";
import GalleryDirectory from "./GalleryDirectory.vue";
import GalleryPicture from "./GalleryPicture.vue";

@Component({
  components: { GallerySearch, GalleryDirectory, GalleryPicture },
})
export default class Gallery extends Vue {
  @Prop(String) readonly pathMatch!: string;

  mounted() {
    this.pathChanged();
  }

  @Watch("pathMatch")
  pathChanged() {
    console.log("Path: ", this.pathMatch);
    this.$galleryStore.setCurrentPath(this.pathMatch);
  }

  // Results of the search (by tags)
  get currentSearch(): Gallery.Item[] {
    const byOperation = this.extractTagsByOperation(this.$uiStore.currentTags);
    const intersection = this.extractIntersection(byOperation);
    const substraction = this.extractSubstraction(byOperation);
    return this.aggregateAll(byOperation, intersection, substraction);
  }

  // ---

  private checkType(type: Gallery.ItemType): boolean {
    return Tools.checkType(this.$galleryStore.currentItem, type);
  }

  private extractTagsByOperation(currentTags: Tag.Search[]): Tag.SearchByOperation {
    let byOperation: Tag.SearchByOperation = {};
    Object.values(Operation).forEach(
      operation => (byOperation[operation] = currentTags.filter(tag => tag.operation === operation))
    );
    return byOperation;
  }

  private extractIntersection(byOperation: Tag.SearchByOperation): Set<Gallery.Item> {
    let intersection = new Set<Gallery.Item>();
    if (byOperation[Operation.INTERSECTION].length > 0) {
      byOperation[Operation.INTERSECTION]
        .map(tag => tag.items)
        .reduce((a, b) => a.filter(c => b.includes(c)))
        .flatMap(items => items)
        .forEach(item => intersection.add(item));
    }
    return intersection;
  }

  private extractSubstraction(byOperation: Tag.SearchByOperation): Set<Gallery.Item> {
    let substraction = new Set<Gallery.Item>();
    if (byOperation[Operation.SUBSTRACTION].length > 0) {
      byOperation[Operation.SUBSTRACTION].flatMap(tag => tag.items).forEach(item => substraction.add(item));
    }
    return substraction;
  }

  private aggregateAll(
    byOperation: Tag.SearchByOperation,
    intersection: Set<Gallery.Item>,
    substraction: Set<Gallery.Item>
  ): Gallery.Item[] {
    byOperation[Operation.ADDITION].flatMap(tag => tag.items).forEach(item => intersection.add(item));
    substraction.forEach(item => intersection.delete(item));
    return [...intersection];
  }
}
</script>

<style lang="scss">
</style>
