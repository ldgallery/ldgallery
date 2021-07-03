<template>
  <b-loading v-if="isLoading" active />
  <MarkDown v-else-if="markdown" class="flex-grow-1" :markdown="markdown" />
</template>

<script lang="ts">
import { MarkDownItem } from "@/@types/gallery";
import { MarkDown } from "@/components/async";
import FetchWithCheck from "@/services/fetchWithCheck";
import { Component, Prop, Vue } from "vue-property-decorator";

@Component({
  components: {
    MarkDown,
  },
})
export default class LdMarkDownViewer extends Vue {
  @Prop({ required: true }) readonly item!: MarkDownItem;

  isLoading: boolean = true;
  markdown: string | null = null;

  created() {
    this.fetchMarkDown();
  }

  get itemResourceUrl(): string {
    return this.$galleryStore.resourceRoot + this.item.properties.resource;
  }

  fetchMarkDown() {
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
      onAction: this.fetchMarkDown,
    });
  }
}
</script>

<style lang="scss" module>
.splashscreen {
  display: flex;
  flex-flow: column;
  align-items: center;
  padding: 32px;
}
.buttonOkay {
  min-width: 310px;
  align-self: center;
}
</style>
