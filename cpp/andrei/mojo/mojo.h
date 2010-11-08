////////////////////////////////////////////////////////////////////////////////
// MOJO: MOving Joint Objects
// Copyright (c) 2002 by Andrei Alexandrescu
//
// Created by Andrei Alexandrescu
//
// Permission to use, copy, modify, distribute and sell this software for any 
//     purpose is hereby granted without fee, provided that the above copyright 
//     notice appear in all copies and that both that copyright notice and this 
//     permission notice appear in supporting documentation.
// The author makes no representations about the suitability of this software 
//     for any purpose. It is provided "as is" 
//     without express or implied warranty.
////////////////////////////////////////////////////////////////////////////////

#ifndef MOJO_H_
#define MOJO_H_

#include <cassert>
#include <iterator>
#include "TypeManip.h"

namespace mojo
{
    // type sugar for constants
    template <class T>
    class constant {
    public:
        explicit constant(const T& obj) : data_(&obj) { }
        const T& get() const
        {
            return *data_;
        }
    private:
        const T* data_;
    };

    // type sugar for temporaries
    template <class T>
    class temporary : private constant<T> {
    public:
        explicit temporary(T& obj) : constant<T>(obj)
        {
        }

        T& get() const
        {
            return const_cast<T&>(constant<T>::get());
        }
    };

    template <class T> 
    class fnresult : public T {
    public:
        // The cast below is valid given that nobody ever really creates a 
        // const fnresult object
        fnresult(const fnresult& rhs): T(temporary<T>(const_cast<fnresult&>(rhs)))
        {
        }

        explicit fnresult(T& rhs) : T(temporary<T>(rhs))
        {
        }
    };

    // CRTP Å¬·¡½º. 
    //
    template <class T> 
    class enabled {
    public:
        operator temporary<T>()
        {
            return temporary<T>(static_cast<T&>(*this));
        }
        operator constant<T>() const
        {
            return constant<T>(static_cast<const T&>(*this));
        }
        operator fnresult<T>()
        {
            return fnresult<T>(static_cast<T&>(*this));
        }

    protected:
        enabled() {} // intended to be derived from
       ~enabled() {} // intended to be derived from
    };

    template <class T>
    struct traits {
        enum { enabled = Loki::SuperSubclassStrict< enabled<T>, T >::value };

        typedef typename 
        Loki::Select<enabled, temporary<T>, T&>::Result temporary;

        typedef typename 
        Loki::Select<enabled, fnresult<T>,   T>::Result fnresult;
    };

    template <class T>
    inline typename traits<T>::temporary as_temporary(T& src) 
    {
        typedef typename traits<T>::temporary temp;
        return temp(src);
    }

    template <class Iter1, class Iter2>
    Iter2 move(Iter1 begin, Iter1 end, Iter2 dest)
    {
        for (; begin != end; ++begin, ++dest)
        {
            *dest = as_temporary(*begin);
        }
        return dest;
    }

    /*
    template <class Iter1, class Iter2>
    Iter2 uninitialized_move(Iter1 begin, Iter1 end, Iter2 dest)
    {
        typedef typename std::iterator_traits<Iter2>::value_type T;

        for (; begin != end; ++begin, ++dest)
        {
            new(*dest) T(as_temporary(*begin));
        }
        return dest;
    }
    */

    //
    // patch by dave abraham
    //
    template <class Iter1, class Iter2>
    Iter2 
    uninitialized_move(Iter1 begin, Iter1 end, Iter2 dest)
    {
        typedef typename std::iterator_traits<Iter2>::value_type T;
        Iter2 built = dest;
        try {
            for (; begin != end; ++begin, ++built) {
                new (&*built) T(as_temporary(*begin));
            }
        } catch (...) {
            for (; dest != built; ++dest) {
                dest->~T();
            }
            throw;
        }
        return built;
    }
}

#endif // MOJO_H_
