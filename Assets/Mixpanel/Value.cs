#if !UNITY_PRO_LICENSE && (UNITY_2_6||UNITY_2_6_1||UNITY_3_0||UNITY_3_0_0||UNITY_3_1||UNITY_3_2||UNITY_3_3||UNITY_3_4||UNITY_3_5||UNITY_4_0||UNITY_4_0_1||UNITY_4_1||UNITY_4_2||UNITY_4_3||UNITY_4_5||UNITY_4_6)
#define DISABLE_MIXPANEL
#warning "Your Unity version does not support native plugins - Mixpanel disabled"
#endif

#if !(UNITY_STANDALONE_OSX || UNITY_STANDALONE_WIN || UNITY_EDITOR || UNITY_IOS || UNITY_ANDROID)
#define DISABLE_MIXPANEL
#warning "Your Unity version does not support native plugins - Mixpanel disabled"
#endif

namespace mixpanel
{
    /// <summary>
    /// A JavaScript/JSON Jelly Bean like value wrapper.
    ///     It can hold dictionaries, arrays, strings, doubles, floats ints and bools.
    ///
    ///         It can easily be converted to JSON via toStyledString();
    ///
    /// example usage:
    ///     var properties = new MPValue();
    /// properties["foo"] = 42;
    /// properties["bar"]["baz"].append(10);
    /// properties["mih"]["meh"]["muh"] = true;
    /// </summary>
    #if !DISABLE_MIXPANEL
    public class Value : detail.Value{
        /// <summary>
        /// construct a Value from a string
        /// </summary>
        public Value(string v):base(v) { }
        /// <summary>
        /// construct a Value from int
        /// </summary>
        public Value(int    v):base(v) { }
        /// <summary>
        /// construct value from double
        /// </summary>
        public Value(double v):base(v) { }
        /// <summary>
        /// construct value from float
        /// </summary>
        public Value(float  v):base(v) { }
        /// <summary>
        /// construct value from bool
        /// </summary>
        public Value(bool   v):base(v) { }
        /// <summary>
        /// default constructor
        /// </summary>
        public Value(        ):base( ) { }

        /// implicit conversion from string
        public static implicit operator Value (string v){ return new Value (v); }
        /// implicit conversion from int
        public static implicit operator Value (int    v){ return new Value (v); }
        /// implicit conversion from double
        public static implicit operator Value (double v){ return new Value (v); }
        /// implicit conversion from float
        public static implicit operator Value (float  v){ return new Value (v); }
        /// implicit conversion from bool
        public static implicit operator Value (bool   v){ return new Value (v); }

        /// implicit convertion to string
        public static implicit operator string (Value v){ return v.asString (); }
        /// implicit conversion to int
        public static implicit operator int    (Value v){ return v.asInt    (); }
        /// implicit conversion to double
        public static implicit operator double (Value v){ return v.asDouble (); }
        /// implicit conversion to float
        public static implicit operator float  (Value v){ return v.asFloat  (); }
        /// implicit conversion to bool
        public static implicit operator bool   (Value v){ return v.asBool   (); }
    };
    #else
    // dummy implementation of the value class - so that the clint code works without native plugin support
    public class Value
    {
        public Value this[int idx]
        {
            get{ return this; }
            set{ }
        }

        public Value this[string idx]
        {
            get{ return this; }
            set{ }
        }

        public Value(string v){ }
        public Value(int    v){ }
        public Value(double v){ }
        public Value(float  v){ }
        public Value(bool   v){ }
        public Value(        ){ }

        public static implicit operator string (Value v){ return v.asString (); }
        public static implicit operator Value (string v){ return new Value (v); }

        public static implicit operator int (Value v){ return v.asInt (); }
        public static implicit operator Value (int v){ return new Value (v); }

        public static implicit operator double (Value v){ return v.asDouble (); }
        public static implicit operator Value (double v){ return new Value (v); }

        public static implicit operator float (Value v){ return v.asFloat (); }
        public static implicit operator Value (float v){ return new Value (v); }

        public static implicit operator bool (Value v){ return v.asBool (); }
        public static implicit operator Value (bool v){ return new Value (v); }

        string asString() { return "";    }
        int    asInt()    { return 0;     }
        double asDouble() { return 0.0;   }
        float  asFloat()  { return 0.0f;  }
        bool   asBool()   { return false; }
    }
    #endif
}

/*! \cond PRIVATE */
namespace mixpanel.detail
{
    // dont's use this class directly, use MPValue instead
    public partial class Value
    {
        public static implicit operator string (Value v){ return v.asString (); }
        public static implicit operator Value (string v){ return new Value (v); }

        public static implicit operator int (Value v){ return v.asInt (); }
        public static implicit operator Value (int v){ return new Value (v); }

        public static implicit operator double (Value v){ return v.asDouble (); }
        public static implicit operator Value (double v){ return new Value (v); }

        public static implicit operator float (Value v){ return v.asFloat (); }
        public static implicit operator Value (float v){ return new Value (v); }

        public static implicit operator bool (Value v){ return v.asBool (); }
        public static implicit operator Value (bool v){ return new Value (v); }

        public detail.Value this[int idx]
        {
            get{ return this.at(idx); }
            set{ this.at(idx).set (value); }
        }

        public detail.Value this[string idx]
        {
            get{ return this.at(idx); }
            set{ this.at(idx).set(value); }
        }
    }
}
/*! \endcond */
