<template>
  <b-loading v-if="isLoading" active />
  <div v-else-if="markdown" :class="$style.splashscreen" class="scrollbar">
    <Markdown :style="config.style" class="flex-grow-1" :markdown="markdown" />
    <b-button size="is-large" :label="buttonAcknowledgeLabel" :class="$style.buttonAcknowledge" @click="validation" />
  </div>
</template>

<script lang="ts">
import { SplashScreenConfig } from "@/@types/splashscreen";
import { Markdown } from "@/components/async";
import FetchWithCheck from "@/services/fetchWithCheck";
import { TranslateResult } from "vue-i18n";
import { Component, Emit, Vue } from "vue-property-decorator";

@Component({
  components: {
    Markdown,
  },
})
export default class SplashScreen extends Vue {
  isLoading: boolean = true;
  markdown: string | null = null;

  get config(): SplashScreenConfig {
    return this.$uiStore.splashScreenConfig!;
  }

  created() {
    this.fetchMarkdown();
  }

  fetchMarkdown() {
    FetchWithCheck.get(`${process.env.VUE_APP_DATA_URL}${this.config.resource}?${this.config.dontshowagainUID ?? ""}`)
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
      type: "is-danger",
      indefinite: true,
      onAction: this.fetchMarkdown,
    });
  }

  get buttonAcknowledgeLabel(): TranslateResult {
    return this.config.buttonAcknowledgeLabel ?? this.$t("splashScreen.button.acknowledge");
  }

  @Emit()
  validation() {}
}
</script>

<style lang="scss" module>
.splashscreen {
  display: flex;
  flex-flow: column;
  align-items: center;
  padding: 32px;
}
.buttonAcknowledge {
  min-width: 310px;
  align-self: center;
}
</style>
