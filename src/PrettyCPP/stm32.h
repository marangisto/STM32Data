#pragma once

////
//
//      STM32 MCU Families
//
///

enum family_t
{{#families}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}}
{{/families}}
    };
