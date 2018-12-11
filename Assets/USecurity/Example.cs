using System;
using UnityEngine;
using UnityEngine.UI;

namespace Assets.USecurity
{
    /// <summary>
    /// Usage example.
    /// </summary>
    public class Example : MonoBehaviour
    {
        public Text Text;

        /// <summary>
        /// Execute on start.
        /// </summary>
        public void Start()
        {
            const string sample = "Hello, world!";

            Text.text = string.Format("Plain text: {0}\nMD5 hash: {1}\nBase64 encoding: {2}\nB64R encoding: {3}\nB64X encryption: {4}\nAES encryption: {5}",
                sample,
                Md5.ComputeHash(sample),
                Base64.Encode(sample),
                B64R.Encode(sample),
                B64X.Encrypt(sample, "password"),
                AES.Encrypt(sample, "password"));
        }
    }
}