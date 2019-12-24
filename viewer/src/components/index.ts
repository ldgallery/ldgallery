import Vue from 'vue'

const requireComponent = require.context(
    '@/components',
    false, // Whether or not to look in subfolders
    // The regular expression used to match base component filenames
    /Ld[A-Z]\w+\.vue$/
)

requireComponent.keys().forEach(fileName => {
    const componentConfig = requireComponent(fileName)
    const componentName = fileName.split('/').pop()!.replace(/\.vue$/, '');

    // Register component globally
    Vue.component(
        componentName,
        // Look for the component options on `.default`, which will
        // exist if the component was exported with `export default`,
        // otherwise fall back to module's root.
        componentConfig.default ?? componentConfig
    )
})