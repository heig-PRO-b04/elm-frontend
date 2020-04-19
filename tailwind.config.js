module.exports = {
  theme: {
    extend: {
      fontFamily: {
        archivo: ["Archivo", "sans-serif"],
      },
      colors: {
        cactus: {
          '050': '#EFFEF9',
          '100': '#E9FEF7',
          '200': '#C7FBE2',
          '300': '#39E4AB',
          '400': '#13D393',
          '500': '#00BF7F',
          '600': '#00A67C',
          '700': '#1A926A',
          '800': '#097F58',
          '900': '#005B3D',
        },
        seaside: {
          '050': '#E1F5FE',
          '100': '#B3E5FC',
          '200': '#81D4FA',
          '300': '#4FC3F7',
          '400': '#29B6F6',
          '500': '#03A9F4',
          '600': '#039BE5',
          '700': '#0288D1',
          '800': '#0277BD',
          '900': '#01579B',
          'A100': '#80D8FF',
          'A200': '#40C4FF',
          'A400': '#00B0FF',
          'A700': '#0091EA',
        },
      },
    },
  },
  variants: {
    borderWidth: ['responsive', 'hover', 'focus'],
    display: ['responsive', 'group-hover'],
    rotate: ['responsive', 'hover', 'focus', 'group-hover'],
    scale: ['responsive', 'hover', 'focus', 'group-hover'],
  },
  plugins: [],
};
