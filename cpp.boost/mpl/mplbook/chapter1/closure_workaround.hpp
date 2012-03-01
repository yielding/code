/*

    Copyright David Abrahams 2003-2004
    Copyright Aleksey Gurtovoy 2003-2004
    Copyright João Abecasis 2004
    
    Distributed under the Boost Software License, Version 1.0. 
    (See accompanying file LICENSE_1_0.txt or copy at 
    http://www.boost.org/LICENSE_1_0.txt)

    Spirit 1.8 doesn't support MSVC 6 and 7, so you have to use Spirit
    1.6.2, which doesn't support closures.  Thus, this file.
*/

#if !defined(BOOST_SPIRIT_CLOSURE_WORKAROUND_HPP)
#define BOOST_SPIRIT_CLOSURE_WORKAROUND_HPP

#include <boost/config.hpp>

#include <stack>

namespace workaround {

    template <typename T>
    struct context
    {
        typedef std::stack<T> stack_type;
        typedef typename stack_type::value_type value_type;

        static stack_type stack_;
        static value_type return_value;

        struct initialize
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.push(value_type());
            }
        };

        struct assign
        {
            void operator()(value_type const & val) const
            {
                stack_.top() = val;
            }

            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.top() = return_value;
            }
        };

        struct add
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.top() += return_value;
            }
        };

        struct subtract
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.top() -= return_value;
            }
        };

        struct multiply
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.top() *= return_value;
            }
        };

        struct divide
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                stack_.top() /= return_value;
            }
        };

        struct return_
        {
            template <typename T1, typename T2>
            void operator()(T1, T2) const
            {
                return_value = stack_.top();
                stack_.pop();
            }
        };
    };

    template <typename T>
    BOOST_DEDUCED_TYPENAME context<T>::stack_type context<T>::stack_ =
        BOOST_DEDUCED_TYPENAME context<T>::stack_type();

    template <typename T>
    BOOST_DEDUCED_TYPENAME context<T>::value_type context<T>::return_value =
        BOOST_DEDUCED_TYPENAME context<T>::value_type();

} // namespace workaround

#define CREATE_CONTEXT(parser)                              \
       boost::spirit::eps_p[ context::initialize() ]        \
    >> (                                                    \
             ( parser )                                     \
           | ( boost::spirit::eps_p[ context::return_() ]   \
               >> nothing_p )                               \
       )                                                    \
    >> boost::spirit::eps_p[ context::return_() ]


#endif // include guard
