<!--
-- ldgallery - A static generator which turns a collection of tagged
--             pictures into a searchable web gallery.
--
-- Copyright (C) 2021  Guillaume FOUET
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
  <b-loading v-if="isLoading" active />
  <Markdown v-else-if="markdown" :class="$style.container" :markdown="markdown" />
</template>

<script lang="ts">
import { MarkdownItem } from "@/@types/gallery";
import { Markdown } from "@/components/async";
import FetchWithCheck from "@/services/fetchWithCheck";
import { Component, Prop, Vue } from "vue-property-decorator";

@Component({
  components: {
    Markdown,
  },
})
export default class LdMarkdownViewer extends Vue {
  @Prop({ required: true }) readonly item!: MarkdownItem;

  isLoading: boolean = true;
  markdown: string | null = null;

  created() {
    this.fetchMarkdown();
  }

  get itemResourceUrl(): string {
    return this.$galleryStore.resourceRoot + this.item.properties.resource;
  }

  // TODO: Identical to SplashScreen.vue, use composition with Vue3.
  fetchMarkdown() {
    FetchWithCheck.get(this.itemResourceUrl)
      .then(response => response.text())
      .then(text => (this.markdown = text))
      .finally(() => (this.isLoading = false))
      .catch(this.displayError);
  }

  displayError(reason: any) {
    this.$buefy.snackbar.open({
      message: `${reason}`,
      actionText: this.$t("snack.retry"),
      position: "is-top",
      type: "is-warning",
      onAction: this.fetchMarkdown,
    });
  }
}
</script>

<style lang="scss" module>
.container {
  padding: 8px;
}
</style>
