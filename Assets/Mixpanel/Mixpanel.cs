/*
    *** do not modify the line below, it is updated by the build scripts ***
    Mixpanel SDK for Unity version v1.0.1
*/

#if !UNITY_PRO_LICENSE && (UNITY_2_6||UNITY_2_6_1||UNITY_3_0||UNITY_3_0_0||UNITY_3_1||UNITY_3_2||UNITY_3_3||UNITY_3_4||UNITY_3_5||UNITY_4_0||UNITY_4_0_1||UNITY_4_1||UNITY_4_2||UNITY_4_3||UNITY_4_5||UNITY_4_6)
#define DISABLE_MIXPANEL
#warning "Your Unity version does not support native plugins - Mixpanel disabled"
#endif

#if !(UNITY_STANDALONE_OSX || UNITY_STANDALONE_WIN || UNITY_EDITOR || UNITY_IOS || UNITY_ANDROID)
#define DISABLE_MIXPANEL
#warning "Your Unity version does not support native plugins - Mixpanel disabled"
#endif

using UnityEngine;
using System;
using System.Text;
using System.Collections.Generic;

namespace mixpanel
{
    /// \example MixpanelExample.cs this example demonstrates usage of the API.

    /// <summary>
    /// Core class for interacting with %Mixpanel Analytics.
    /// </summary>
    /// <description>
    /// <p>Create a GameObject and attach this %Mixpanel component. Then, set the properties in the unity inspector (token, debug token, etc.)</p>
    /// <p>Use the Mixpanel class to set up your project and track events in %Mixpanel Engagement. Once you have
    /// a component, you can track events in %Mixpanel Engagement using <c>Mixpanel.Track(string eventName)</c>.
    /// You can also update %People Analytics records with Mixpanel.people. </p>
    /// </description>
    /// <code>
    ///        //Track an event in Mixpanel Engagement<br/>
    ///        Mixpanel.track("Hello World");<br/>
    ///        Mixpanel.Identify("CURRENT USER DISTINCT ID");<br/>
    ///        Mixpanel.people.Set("Plan", "Premium");<br/>
    /// </code>
    public class Mixpanel : MonoBehaviour
    {
        /*! \cond PRIVATE */
        #region settings
        [Header("Project")]
        [Tooltip("The token of the Mixpanel project.")]
        public string token = "";
        [Tooltip("Used when the DEBUG compile flag is set or when in the editor. Useful if you want to use different tokens for test builds.")]
        public string debugToken = "";

        [Header("Debugging")]
        [Tooltip("Also send out data when inside the Unity editor.")]
        public bool trackInEditor = false;
        [Tooltip("The minimum log level you're interested in. If set to LL_NONE, logging will be disabled.")]
        public detail.Mixpanel.LogEntry.Level minLogLevel = detail.Mixpanel.LogEntry.Level.LL_WARNING;

        [Header("Configuration")]
        [Tooltip("How frequently (in seconds) to send data to Mixpanel.")]
        [Range(1, 600)]
        public int flushInterval = 60;
        // [Tooltip("Data will be discarded if the outgoing queue grows above this size (in megabytes).")]
        private int maxQueueSizeInMB = 5;
        // [Tooltip("Set the automatic $ios_ifa property. For this to work, you also have to define the MIXPANEL_USE_IOS_IFA script symbol in the player settings. Make sure to only enable this if your app actually shows ads, otherwise your app will be rejected by Apple. If you don't enable it, you may delete Plugins/iOS/iOSIdentifiers.mm (to be on the safe side).")]
        private bool useIosIfa = false;
        #endregion
        /*! \endcond */

        /// <summary>
        /// Sets the distinct ID of the current user.
        /// </summary>
        /// <param name="uniqueId">a string uniquely identifying this user. Events sent to %Mixpanel
        /// using the same disinct_id will be considered associated with the same visitor/customer for
        /// retention and funnel reporting, so be sure that the given value is globally unique for each
        /// individual user you intend to track.
        /// </param>
        public static void Identify(string uniqueId)
        {
            if (tracking_enabled)
                instance.identify(uniqueId);
        }

        /// <summary>
        /// Creates a distinct_id alias.
        /// </summary>
        /// <param name="alias">the new distinct_id that should represent original</param>
        public static void Alias(string alias)
        {
            if (tracking_enabled)
                instance.alias(alias);
        }

        /// <summary>
        /// Clear all super properties.
        /// </summary>
        public static void ClearSuperProperties()
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.clear_super_properties();
            #endif
        }

        /// <summary>
        /// Clears all current event timers.
        /// </summary>
        public static void ClearTimedEvents()
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.clear_timed_events();
            #endif
        }

        /// <summary>
        /// Clears the event timer for a single event.
        /// </summary>
        /// <param name="eventName">the name of event to clear event timer</param>
        public static bool ClearTimedEvent(string eventName)
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                return instance.clear_timed_event(eventName);
            #endif
            return false;
        }

        /// <summary>
        ///  Uploads queued data to the %Mixpanel server.
        /// </summary>
        public static void FlushQueue()
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.flush_queue();
            #endif
        }

        /// <summary>
        /// Registers super properties, overwriting ones that have already been set.
        /// </summary>
        /// <param name="key">name of the property to register</param>
        /// <param name="value">value of the property to register</param>
        public static void Register(string key, Value value) {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.register_(key, value);
            #endif
        }

        /// <summary>
        /// Registers super properties without overwriting ones that have already been set.
        /// </summary>
        /// <param name="key">name of the property to register</param>
        /// <param name="value">value of the property to register</param>
        public static bool RegisterOnce(string key, Value value) {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                return instance.register_once(key, value);
            #endif
            return false;
        }

        /// <summary>
        /// Clears all distinct_ids, superProperties, and push registrations from persistent storage.
        /// Will not clear referrer information.
        /// </summary>
        public static void Reset()
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.reset();
            #endif
        }

        /// <summary>
        /// Start timing of an event. Calling Mixpanel.StartTimedEvent(string eventName) will not send an event,
        /// but when you eventually call Mixpanel.Track(string eventName), your tracked event will be sent with a "$duration" property,
        /// representing the number of seconds between your calls.
        /// </summary>
        /// <param name="eventName">the name of the event to track with timing</param>
        public static bool StartTimedEvent(string eventName)
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                return instance.start_timed_event(eventName);
            #endif
            return false;
        }

        /// <summary>
        /// Begin timing of an event, but only if the event has not already been registered as a timed event.
        /// Useful if you want to know the duration from the point in time the event was first registered.
        /// </summary>
        /// <param name="eventName">the name of the event to track with timing</param>
        public static bool StartTimedEventOnce(string eventName)
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                return instance.start_timed_event_once(eventName);
            #endif
            return false;
        }

        /// <summary>
        /// Tracks an event.
        /// </summary>
        /// <param name="eventName">the name of the event to send</param>
        public static void Track(string eventName)
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.track(eventName, new Value());
            #endif
        }

        /// <summary>
        /// Tracks an event with properties.
        /// </summary>
        /// <param name="eventName">the name of the event to send</param>
        /// <param name="properties">A JSONObject containing the key value pairs of the properties
        /// to include in this event. Pass null if no extra properties exist.
        /// </param>
        public static void Track(string eventName, Value properties)
        {
            #if !DISABLE_MIXPANEL
            if (tracking_enabled)
                instance.track(eventName, properties);
            #endif
        }

        /// <summary>
        /// Removes a single superProperty.
        /// </summary>
        /// <param name="key">name of the property to unregister</param>
        public static bool Unregister(string key) {
            if (tracking_enabled)
                return instance.unregister(key);
            return false;
        }

        /// <summary>
        /// Core interface for using %Mixpanel %People Analytics features. You can get an instance by calling Mixpanel.people
        /// </summary>
        public class People
        {

            /// <summary>
            /// Append values to list properties.
            /// </summary>
            /// <param name="properties">mapping of list property names to values to append</param>
            public void Append(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.append_properties(properties);
                #endif
            }

            /// <summary>
            /// Appends a value to a list-valued property.
            /// </summary>
            /// <param name="listName">the %People Analytics property that should have it's value appended to</param>
            /// <param name="value">the new value that will appear at the end of the property's list</param>
            public void Append(string listName,  Value value)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.append(listName, value);
                #endif
            }

            /// <summary>
            /// Permanently clear the whole transaction history for the identified people profile.
            /// </summary>
            public void ClearCharges()
            {
                if (tracking_enabled)
                    mixpanel.people.clear_charges();
            }

            /// <summary>
            /// Change the existing values of multiple %People Analytics properties at once.
            /// </summary>
            /// <param name="properties"> A map of String properties names to Long amounts. Each property associated with a name in the map </param>
            public void Increment(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.increment_properties(properties);
                #endif
            }

            /// <summary>
            /// Convenience method for incrementing a single numeric property by the specified amount.
            /// </summary>
            /// <param name="property">property name</param>
            /// <param name="by">amount to increment by</param>
            public void Increment(string property,  Value by)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.increment(property, by);
                #endif
            }


            /// <summary>
            /// Set a collection of properties on the identified user all at once.
            /// </summary>
            /// <param name="properties">a JSONObject containing the collection of properties you wish to apply
            /// to the identified user. Each key in the JSONObject will be associated with a property name, and the value
            /// of that key will be assigned to the property.
            /// </param>
            public void Set(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.set_properties(properties);
                #endif
            }

            /// <summary>
            /// Sets a single property with the given name and value for this user.
            /// </summary>
            /// <param name="property">property name</param>
            /// <param name="to">property value</param>
            public void Set(string property,  Value to)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.set(property, (detail.Value)to);
                #endif
            }

            /// <summary>
            /// Like Mixpanel.Set(string property, Value to), but will not set properties that already exist on a record.
            /// </summary>
            /// <param name="property">property name</param>
            /// <param name="to">property value</param>
            public void SetOnce(string property,  Value to)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.set_once(property, to);
                #endif
            }

            /// <summary>
            /// Like Mixpanel.Set(Value properties), but will not set properties that already exist on a record.
            /// </summary>
            /// <param name="properties">a JSONObject containing the collection of properties you wish to apply to the identified user. Each key in the JSONObject will be associated with a property name, and the value of that key will be assigned to the property.</param>
            public void SetOnce(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.set_once_properties(properties);
                #endif
            }

            /// <summary>
            /// Track a revenue transaction for the identified people profile.
            /// </summary>
            /// <param name="amount">amount of revenue received</param>
            public void TrackCharge(double amount)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.track_charge(amount, new Value());
                #endif
            }

            /// <summary>
            /// Adds values to a list-valued property only if they are not already present in the list.
            /// </summary>
            /// <param name="listName">name of the list-valued property to set or modify</param>
            /// <param name="values">an array of values to add to the property value if not already present</param>
            public void TrackCharge(double amount, Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.track_charge(amount, properties);
                #endif
            }

            /// <summary>
            /// Adds values to a list-valued property only if they are not already present in the list.
            /// If the property does not currently exist, it will be created with the given list as it's value.
            /// If the property exists and is not list-valued, the union will be ignored.
            /// </summary>
            /// <param name="properties">mapping of list property names to lists to union</param>
            public void Union(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.union_properties(properties);
                #endif
            }

            /// <summary>
            /// Adds values to a list-valued property only if they are not already present in the list.
            /// If the property does not currently exist, it will be created with the given list as it's value.
            /// If the property exists and is not list-valued, the union will be ignored.            /// </summary>
            /// <param name="listName">name of the list-valued property to set or modify</param>
            /// <param name="values">an array of values to add to the property value if not already present</param>
            public void Union(string listName,  Value values)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.union_(listName, values);
                #endif
            }

            /// <summary>
            /// Remove a list of properties and their values from the current user's profile in %Mixpanel %People.
            /// </summary>
            /// <param name="properties">properties array</param>
            public void Unset(Value properties)
            {
                #if !DISABLE_MIXPANEL
                if (tracking_enabled)
                    mixpanel.people.unset_properties(properties);
                #endif
            }

            /// <summary>
            /// Takes a string property name, and permanently removes the property and their values from a profile.
            /// </summary>
            /// <param name="property">property</param>
            public void Unset(string property)
            {
                if (tracking_enabled)
                    mixpanel.people.unset(property);
            }

            /// <summary>
            /// Sets the email for this user.
            /// </summary>
            public string Email
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_email(value);
                }
            }

            /// <summary>
            /// Sets the first name for this user.
            /// </summary>
            public string FirstName
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_first_name(value);
                }
            }

            /// <summary>
            /// Sets the last name for this user.
            /// </summary>
            public string LastName
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_last_name(value);
                }
            }

            /// <summary>
            /// Sets the name for this user.
            /// </summary>
            public string Name
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_name(value);
                }
            }

            /// <summary>
            /// Sets the phone number for this user.
            /// </summary>
            public string Phone
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_phone(value);
                }
            }

            #if UNITY_IOS
            /// <summary>
            /// Register the given device to receive push notifications.
            /// </summary>
            public byte[] PushDeviceToken
            {
                set
                {
                    if (tracking_enabled)
                    {
                        mixpanel.people.set_push_id(System.BitConverter.ToString(value).ToLower().Replace("-", ""));
                    }
                }
            }
            #else
            /// <summary>
            /// Register the given device to receive push notifications.
            /// </summary>
            public string PushDeviceToken
            {
                set
                {
                    if (tracking_enabled)
                        mixpanel.people.set_push_id(value);
                }
            }
            #endif

            #region internal
            mixpanel.detail.Mixpanel mixpanel;
            bool tracking_enabled;
            public People(mixpanel.detail.Mixpanel mixpanel, bool tracking_enabled)
            {
                this.mixpanel = mixpanel;
                this.tracking_enabled = tracking_enabled;
            }
            #endregion
        }

        /// <summary>
        /// Return an accessor for %Mixpanel people with a temporary distinct id.
        /// </summary>
        public static People people
        {
            get
            {
                if (people_ == null)
                    people_ = new People(instance, tracking_enabled);
                return people_;
            }
        }

        #region internal
        static mixpanel.detail.Mixpanel mp_interface;
        private static mixpanel.detail.Mixpanel instance
        {
            get
            {
                return mp_interface;
            }
        }

        static bool tracking_enabled = true;

        void Awake()
        {
            DontDestroyOnLoad(this);

            var reporter = gameObject.AddComponent<IntegrationReporter>();
            reporter.token = token;

            #if UNITY_EDITOR
            tracking_enabled = trackInEditor;
            #endif

            #if DISABLE_MIXPANEL
            tracking_enabled = false;
            Debug.LogWarning("Your Unity version does not support native plaugins. Disabling Mixpanel.");
            #endif

            if (tracking_enabled && mp_interface == null) {
                mp_interface = new mixpanel.detail.Mixpanel(
                    #if DEBUG || UNITY_EDITOR
                        token:debugToken,
                    #else
                        token:token,
                    #endif
                    distinct_id:mixpanel.platform.MixpanelUnityPlatform.get_distinct_id(),
                    storage_directory:mixpanel.platform.MixpanelUnityPlatform.get_storage_directory(),
                    enable_log_queue:true
                );
                mp_interface.set_minimum_log_level(minLogLevel);
                mp_interface.set_maximum_queue_size((uint)(maxQueueSizeInMB * 1024 * 1024));

                // these are properties that are difficult to obtain only via native code, so we set the as super properties
                Register("$screen_width", Screen.width);
                Register("$screen_height", Screen.height);
                Register("$screen_dpi", Screen.dpi);

                #if UNITY_ANDROID && !UNITY_EDITOR
                Register("$app_build_number", platform.MixpanelUnityPlatform.get_android_version_name());
                Register("$app_version_string", platform.MixpanelUnityPlatform.get_android_version_code());

                people.Set("$android_app_version_string", platform.MixpanelUnityPlatform.get_android_version_name());
                people.Set("$android_app_build_number", platform.MixpanelUnityPlatform.get_android_version_code());
                #endif

                #if UNITY_IOS
                if (useIosIfa && !detail.IOSIdentifiers.MIXPANEL_USE_IOS_IFA_ENABLED)
                {
                    Debug.LogError("Mixpanel: If you want to use the automatic $ios_ifa property, you have to define the MIXPANEL_USE_IOS_IFA script symbol in the player settings."+
                                   "Only enable this, if your app shows advertising, otherwise your app might get rejected or pulled from the store by apple. If your app is "+
                                   "not showing ads disable useIosIfa, remove MIXPANEL_USE_IOS_IFA and delete Plugins/iOS/iOSIdentifiers.mm.");
                }
                if (!useIosIfa && detail.IOSIdentifiers.MIXPANEL_USE_IOS_IFA_ENABLED)
                {
                    Debug.LogError("Mixpanel: You have requested to not use $ios_ifa, but still have the MIXPANEL_USE_IOS_IFA scripting symbol defined. If your app is not showing ads remove MIXPANEL_USE_IOS_IFA and delete Plugins/iOS/iOSIdentifiers.mm");
                }

                #if MIXPANEL_USE_IOS_IFA && !UNITY_EDITOR
                if (useIosIfa) // running on device, enabled via settings and scripting symbol is defined - we can use it.
                {
                    var idfa = detail.IOSIdentifiers.mixpanel_ios_get_idfa();
                    Register("$ios_ifa", idfa);
                    people.Set("$ios_ifa", idfa);
                }
                #endif

                #endif

                if (flushInterval < 0)
                {
                    Debug.LogError("batchSendInterval must be greater or equal zo zero");
                    flushInterval = 0;
                }

                mp_interface.set_flush_interval((uint)flushInterval);
            }
        }

        void OnDestroy()
        {
            if (tracking_enabled)
            {
                mp_interface.Dispose();
            }
        }

        NetworkReachability reachability = NetworkReachability.ReachableViaLocalAreaNetwork;
        void Update()
        {
            if (tracking_enabled)
            {
                detail.Mixpanel.LogEntry le = new mixpanel.detail.Mixpanel.LogEntry();

                while (mp_interface.get_next_log_entry(le))
                {
                    string msg = string.Format("Mixpanel[{0}]: {1}", le.level, le.message);
                    switch(le.level)
                    {
                        case detail.Mixpanel.LogEntry.Level.LL_ERROR: Debug.LogError(msg); break;
                        case detail.Mixpanel.LogEntry.Level.LL_WARNING: Debug.LogWarning(msg); break;
                        default: Debug.Log(msg); break;
                    }
                }

                if (reachability != Application.internetReachability)
                {
                    reachability = Application.internetReachability;
                    switch(reachability)
                    {
                        case NetworkReachability.NotReachable: mp_interface.on_reachability_changed(mixpanel.detail.Mixpanel.NetworkReachability.NotReachable); break;
                        case NetworkReachability.ReachableViaCarrierDataNetwork: mp_interface.on_reachability_changed(mixpanel.detail.Mixpanel.NetworkReachability.ReachableViaCarrierDataNetwork); break;
                        case NetworkReachability.ReachableViaLocalAreaNetwork: mp_interface.on_reachability_changed(mixpanel.detail.Mixpanel.NetworkReachability.ReachableViaLocalAreaNetwork); break;
                    }
                }
            }
        }

        static People people_;
        #endregion
    }

    class IntegrationReporter : MonoBehaviour {
        public string token = null;

        void Update() {
            if (token == null || token.Length == 0) return;

            enabled = false;
            string url = BuildRequestURL();
            var request = new WWW(url);
            StartCoroutine(WaitForRequest(request));
        }

        string BuildRequestURL() {
            string body = "{\"event\":\"Integration\",\"properties\":{\"token\":\"85053bf24bba75239b16a601d9387e17\",\"mp_lib\":\"unity\",\"distinct_id\":\"" + this.token +"\"}}";
            byte[] bytes = Encoding.UTF8.GetBytes(body);
            string encoded = Convert.ToBase64String(bytes);
            return "https://api.mixpanel.com/track/?data=" + encoded;
        }

        IEnumerator<WWW> WaitForRequest(WWW request) {
            yield return request;

            if (request.error != null) {
                enabled = true;
            }
        }
    }
}
