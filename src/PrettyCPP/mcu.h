#pragma once

////
//
//      {{family}} MCUs
//
///

enum mcu_t
{{#mcus}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{refName}}
{{/mcus}}
    };

enum svd_t
{{#svds}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/svds}}
    };

enum gpio_conf_t
{{#ipGPIOs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{enumValue}}
{{/ipGPIOs}}
    };

enum periph_t
{{#periphs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/periphs}}
    };

template<mcu_t MCU> struct mcu_traits {};

{{#mcus}}
template<> struct mcu_traits<{{refName}}>
{
    static constexpr family_t family = {{family}};
    static constexpr svd_t svd = {{svd}};
    static constexpr gpio_conf_t gpio_conf = {{gpioConf}};
};

{{/mcus}}
// Set target from command-line -DMCU= option
static constexpr mcu_t target = MCU;

