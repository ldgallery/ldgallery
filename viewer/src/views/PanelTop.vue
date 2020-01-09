<template>
  <div v-if="isReady">
    <b-steps
      v-model="activeStep"
      class="pathBreadcrumb"
      type="is-info"
      :has-navigation="false"
      :animated="false"
      @input="onStepClick"
    >
      <b-step-item
        v-for="item in $galleryStore.currentItemPath"
        :key="item.path"
        :label="item.title"
        :icon="getIcon(item)"
        :to="item.path"
      />
    </b-steps>
  </div>
</template>

<script lang="ts">
import { Component, Vue, Prop, Watch } from "vue-property-decorator";
import Gallery from "./Gallery.vue";

@Component
export default class PanelTop extends Vue {
  activeStep: number = -1;

  mounted() {
    this.currentItemPathChanged();
  }

  get isReady() {
    return this.activeStep >= 0;
  }

  @Watch("$galleryStore.currentItemPath")
  currentItemPathChanged() {
    this.activeStep = -1;
    this.$nextTick(() => (this.activeStep = this.$galleryStore.currentItemPath.length - 1));
  }

  getIcon(item: Gallery.Item) {
    switch (item.properties.type) {
      case "picture":
        return "image";
      case "directory":
        return "folder";
    }
  }

  onStepClick(index: number) {
    const item = this.$galleryStore.currentItemPath[index];
    if (item) this.$router.push(item.path);
  }
}
</script>

<style lang="scss">
.pathBreadcrumb {
  margin: 3px;
}
.step-title {
  color: white;
}
</style>
