using UnityEngine;

using System;
using System.Collections;

namespace Nanome.Core.Extension
{

    public struct Tuple<T1, T2>
    {

        public T1 Item1;
        public T2 Item2;

        internal Tuple(T1 item1, T2 item2)
        {
            Item1 = item1;
            Item2 = item2;
        }

        public override string ToString()
        {
            return "("
                + Item1.ToString() + ", "
                + Item2.ToString()
                + ")";
        }

    }

    public struct Tuple<T1, T2, T3>
    {

        public T1 Item1;
        public T2 Item2;
        public T3 Item3;

        internal Tuple(T1 item1, T2 item2, T3 item3)
        {
            Item1 = item1;
            Item2 = item2;
            Item3 = item3;
        }

        public override string ToString()
        {
            return "("
                + Item1.ToString() + ", "
                + Item2.ToString() + ", "
                + Item3.ToString()
                + ")";
        }

    }

    public struct Tuple<T1, T2, T3, T4>
    {

        public T1 Item1;
        public T2 Item2;
        public T3 Item3;
        public T4 Item4;

        internal Tuple(T1 item1, T2 item2, T3 item3, T4 item4)
        {
            Item1 = item1;
            Item2 = item2;
            Item3 = item3;
            Item4 = item4;
        }

        public override string ToString()
        {
            return "("
                + Item1.ToString() + ", "
                + Item2.ToString() + ", "
                + Item3.ToString() + ", "
                + Item4.ToString()
                + ")";
        }

    }

    public struct Tuple<T1, T2, T3, T4, T5>
    {

        public T1 Item1;
        public T2 Item2;
        public T3 Item3;
        public T4 Item4;
        public T5 Item5;

        internal Tuple(T1 item1, T2 item2, T3 item3, T4 item4, T5 item5)
        {
            Item1 = item1;
            Item2 = item2;
            Item3 = item3;
            Item4 = item4;
            Item5 = item5;
        }

        public override string ToString()
        {
            return "("
                + Item1.ToString() + ", "
                + Item2.ToString() + ", "
                + Item3.ToString() + ", "
                + Item4.ToString() + ", "
                + Item5.ToString()
                + ")";
        }

    }

    public static class Tuple
    {

        public static Tuple<T1, T2> Create<T1, T2>(T1 first, T2 second)
        {
            return new Tuple<T1, T2>(first, second);
        }

        public static Tuple<T1, T2, T3> Create<T1, T2, T3>(T1 first, T2 second, T3 third)
        {
            return new Tuple<T1, T2, T3>(first, second, third);
        }

        public static Tuple<T1, T2, T3, T4> Create<T1, T2, T3, T4>(T1 first, T2 second, T3 third, T4 fourth)
        {
            return new Tuple<T1, T2, T3, T4>(first, second, third, fourth);
        }

        public static Tuple<T1, T2, T3, T4, T5> Create<T1, T2, T3, T4, T5>(T1 first, T2 second, T3 third, T4 fourth, T5 fifth)
        {
            return new Tuple<T1, T2, T3, T4, T5>(first, second, third, fourth, fifth);
        }

    }

}
