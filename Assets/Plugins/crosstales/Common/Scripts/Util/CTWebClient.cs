#if !UNITY_WSA || UNITY_EDITOR
namespace Crosstales.Common.Util
{
    /// <summary>Specialized WebClient.</summary>
    public class CTWebClient : System.Net.WebClient
    {
        #region Properties

        /// <summary>Timeout in milliseconds</summary>
        public int Timeout { get; set; }

        /// <summary>Connection limit for all WebClients</summary>
        public int ConnectionLimit { get; set; }

        #endregion


        #region Constructors

        public CTWebClient() : this(5000) { }

        public CTWebClient(int timeout, int connectionLimit = 20)
        {
            Timeout = timeout;
            ConnectionLimit = connectionLimit;
        }

        #endregion


        #region Public methods

        public System.Net.WebRequest CTGetWebRequest(string uri)
        {
            return GetWebRequest(new System.Uri(uri));
        }

        #endregion


        #region Overriden methods

        protected override System.Net.WebRequest GetWebRequest(System.Uri uri)
        {
            System.Net.WebRequest wr = base.GetWebRequest(uri);

            if (wr.GetType() == typeof(System.Net.HttpWebRequest))
            {
                System.Net.HttpWebRequest request = (System.Net.HttpWebRequest)base.GetWebRequest(uri);

                if (request != null)
                {
                    request.ServicePoint.ConnectionLimit = ConnectionLimit;
                    request.Timeout = Timeout;

                    return request;
                }
            }

            return wr;
        }

        #endregion
    }
}
#endif
// © 2017-2019 crosstales LLC (https://www.crosstales.com)