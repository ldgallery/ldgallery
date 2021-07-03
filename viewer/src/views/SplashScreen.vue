<template>
  <div :class="$style.splashscreen" class="scrollbar">
    <div :style="config.style" :class="$style.markdown" class="flex-grow-1" v-html="splashScreenMd" />
    <b-button size="is-large" :label="buttonValidateLabel" :class="$style.buttonOkay" @click="validation" />
  </div>
</template>

<script lang="ts">
import { SplashScreenConfig } from "@/@types/splashscreen";
import marked from "marked";
import { TranslateResult } from "vue-i18n";
import { Component, Emit, Vue } from "vue-property-decorator";

@Component
export default class SplashScreen extends Vue {
  get config(): SplashScreenConfig {
    return this.$uiStore.splashScreenConfig!;
  }

  get splashScreenMd(): string {
    return marked(this.$uiStore.splashScreenData!);
  }

  get buttonValidateLabel(): TranslateResult {
    return this.config.buttonValidateLabel ?? this.$t("splashScreen.button.validation");
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
.buttonOkay {
  min-width: 310px;
  align-self: center;
}

.markdown {
  line-height: 1.7;
  word-wrap: break-word;

  a {
    color: #9bcdff;
    text-decoration: none;
  }

  hr {
    background-color: #666;
  }

  p,
  blockquote,
  ul,
  ol,
  dl,
  table,
  pre {
    margin: 15px 0;
  }

  ul,
  ol {
    padding-left: 30px;
  }

  h1 {
    border-bottom: 1px solid #888;
    font-size: 2.5em;
  }

  h2 {
    border-bottom: 1px solid #666;
    font-size: 2em;
  }

  h3 {
    font-size: 1.5em;
  }

  h4 {
    font-size: 1.2em;
  }

  h5 {
    font-size: 1em;
  }

  h6 {
    color: #777;
    font-size: 1em;
  }

  h1,
  h2,
  h3,
  h4,
  h5,
  h6 {
    font-weight: bold;
    margin: 1em 0 15px 0;
  }

  h1 + p,
  h2 + p,
  h3 + p {
    margin-top: 10px;
  }

  pre {
    color: white;
    background-color: #2e4049;
  }

  code {
    @extend pre;
    font-family: Consolas, "Liberation Mono", Courier, monospace;
    font-size: 0.8em;
    white-space: pre;
  }
}
</style>
