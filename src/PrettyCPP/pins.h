#pragma once

////
//
//      {{family}} pins
//
///
{{#gpio}}

enum port_t
{{#ports}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/ports}}
    };

enum pin_t
{{#pins}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/pins}}
    };

enum alt_fun_t
{{#afs}}
    {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}}
{{/afs}}
    };
{{/gpio}}

