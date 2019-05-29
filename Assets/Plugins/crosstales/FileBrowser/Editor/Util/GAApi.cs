namespace Crosstales.FB.EditorUtil
{
    /// <summary>GA-wrapper API.</summary>
    public abstract class GAApi : Common.EditorUtil.BaseGAApi
    {

        #region Public methods

        /// <summary>Tracks an event from the asset.</summary>
        /// <param name="category">Specifies the event category.</param>
        /// <param name="action">Specifies the event action.</param>
        /// <param name="label">Specifies the event label.</param>
        /// <param name="value">Specifies the event value.</param>
        public static void Event(string category, string action, string label = "", int value = 0)
        {
            if (EditorConfig.TRACER)
            {
                Event(Util.Constants.ASSET_NAME, Util.Constants.ASSET_VERSION, category, action, label, value);
            }
        }

        #endregion

    }
}
// © 2017-2019 crosstales LLC (https://www.crosstales.com)