const plugin = require('tailwindcss/plugin');
const defaultTheme = require('tailwindcss/defaultTheme');

module.exports = {
    mode: 'jit',
    theme: {
        extend: {
            fontFamily: {
                'sans': ['Inter', ...defaultTheme.fontFamily.sans],
            },
        },
    },
    content: [
        "Web/Element/**/*.hs",
        "Web/View/**/*.hs",
    ],
    safelist: [
        // Add custom class names.
        // https://tailwindcss.com/docs/content-configuration#safelisting-classes
    ],
    plugins: [
        require('@tailwindcss/forms'),
        require('@tailwindcss/typography'),
    ],
};
