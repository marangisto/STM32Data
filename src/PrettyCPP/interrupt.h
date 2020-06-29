#pragma once

////
//
//      {{family}} interrupts
//
///

{{#interrupt}}
struct interrupt
{
    static inline void enable() { __asm volatile ("cpsie i"); }
    static inline void disable() { __asm volatile ("cpsid i"); }

    enum interrupt_t
{{#interrupts}}
{{^pad}}
        {{#first}}{ {{/first}}{{^first}}, {{/first}}{{name}} = {{value}} // {{description}}
{{/pad}}
{{/interrupts}}
    };

    template<interrupt_t INTERRUPT>
    static bool get()
    {
        return helper<nvic_t, INTERRUPT>::get();
    }

    template<interrupt_t INTERRUPT>
    static void set()
    {
        helper<nvic_t, INTERRUPT>::set();
    }

    template<interrupt_t INTERRUPT>
    static void clear()
    {
        helper<nvic_t, INTERRUPT>::clear();
    }

    template<interrupt_t INTERRUPT>
    static bool get_pending()
    {
        return helper<nvic_t, INTERRUPT>::get_pending();
    }

    template<interrupt_t INTERRUPT>
    static void set_pending()
    {
        helper<nvic_t, INTERRUPT>::set_pending();
    }

    template<interrupt_t INTERRUPT>
    static void clear_pending()
    {
        helper<nvic_t, INTERRUPT>::clear_pending();
    }

    template<typename NVIC, interrupt_t I, typename = is_in_range<true>>
    struct helper
    {
        static_assert(always_false_i<I>::value, "no such interrupt");
    };
{{#registers}}

    template<typename NVIC, interrupt_t I>
    struct helper<NVIC, I, is_in_range<({{lbound}} <= I && I < {{hbound}})>>
    {
        static bool get() { return NVIC::V.ISER{{suffix}} & 1 << (I - {{lbound}}); }
        static void set() { NVIC::V.ISER{{suffix}} = 1 << (I - {{lbound}}); }
        static void clear() { NVIC::V.ICER{{suffix}} = 1 << (I - {{lbound}}); }
        static bool get_pending() { return NVIC::V.ISPR{{suffix}} & 1 << (I - {{lbound}}); }
        static void set_pending() { NVIC::V.ISPR{{suffix}} = 1 << (I - {{lbound}}); }
        static void clear_pending() { NVIC::V.ICPR{{suffix}} = 1 << (I - {{lbound}}); }
    };
{{/registers}}
};
{{/interrupt}}

