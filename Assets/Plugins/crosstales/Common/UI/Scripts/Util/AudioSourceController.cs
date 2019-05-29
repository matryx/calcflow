using UnityEngine;
using UnityEngine.UI;

namespace Crosstales.UI.Util
{
    /// <summary>Controller for AudioSources.</summary>
    //[HelpURL("https://www.crosstales.com/media/data/assets/radio/api/class_crosstales_1_1_radio_1_1_demo_1_1_audio_source_controller.html")]
    public class AudioSourceController : MonoBehaviour
    {
        #region Variables

        [Header("Audio Sources")]
        /// <summary>Searches for all AudioSource in the whole scene (default: true).</summary>
        [Tooltip("Searches for all AudioSource in the whole scene (default: true).")]
        public bool FindAllAudioSourcesOnStart = true;

        /// <summary>Active controlled AudioSources.</summary>
        [Tooltip("Active controlled AudioSources.")]
        public AudioSource[] AudioSources;

        [Header("Settings")]
        /// <summary>Resets all active AudioSources (default: true).</summary>
        [Tooltip("Resets all active AudioSources (default: true).")]
        public bool ResetAudioSourcesOnStart = true;

        /// <summary>Mute on/off (default: false).</summary>
        [Tooltip("Mute on/off (default: false).")]
        public bool Mute = false;

        /// <summary>Loop on/off (default: false).</summary>
        [Tooltip("Loop on/off (default: false).")]
        public bool Loop = false;

        /// <summary>Volume of the audio (default: 1)</summary>
        [Tooltip("Volume of the audio (default: 1)")]
        public float Volume = 1f;

        /// <summary>Pitch of the audio (default: 1).</summary>
        [Tooltip("Pitch of the audio (default: 1).")]
        public float Pitch = 1f;

        /// <summary>Stereo pan of the audio (default: 0).</summary>
        [Tooltip("Stereo pan of the audio (default: 0).")]
        public float StereoPan = 0f;

        [Header("UI Objects")]
        public Text VolumeText;
        public Text PitchText;
        public Text StereoPanText;

        private bool initalized = false;

        #endregion


        #region MonoBehaviour methods

        public void Update()
        {
            if (!initalized && Time.frameCount % 30 == 0)
            {
                initalized = true;

                if (FindAllAudioSourcesOnStart)
                {
                    FindAllAudioSources();
                }

                if (ResetAudioSourcesOnStart)
                {
                    ResetAllAudioSources();
                }
            }
        }

        #endregion


        #region Public methods

        /// <summary>Finds all audio sources in the scene.</summary>
        public void FindAllAudioSources()
        {
            AudioSources = FindObjectsOfType(typeof(AudioSource)) as AudioSource[];
        }

        /// <summary>Resets all audio sources.</summary>
        public void ResetAllAudioSources()
        {
            MuteEnabled(Mute);
            LoopEnabled(Loop);
            VolumeChanged(Volume);
            PitchChanged(Pitch);
            StereoPanChanged(0f);
        }

        public void MuteEnabled(bool enabled)
        {
            foreach (AudioSource source in AudioSources)
            {
                source.mute = enabled;
            }
        }

        public void LoopEnabled(bool enabled)
        {
            foreach (AudioSource source in AudioSources)
            {
                source.mute = enabled;
            }
        }

        public void VolumeChanged(float value)
        {
            foreach (AudioSource source in AudioSources)
            {
                source.volume = value;
            }

            if (VolumeText != null)
            {
                VolumeText.text = value.ToString(Common.Util.BaseConstants.FORMAT_TWO_DECIMAL_PLACES);
            }
        }

        public void PitchChanged(float value)
        {
            foreach (AudioSource source in AudioSources)
            {
                source.pitch = value;
            }

            if (PitchText != null)
            {
                PitchText.text = value.ToString(Common.Util.BaseConstants.FORMAT_TWO_DECIMAL_PLACES);
            }
        }

        public void StereoPanChanged(float value)
        {
            foreach (AudioSource source in AudioSources)
            {
                source.panStereo = value;
            }

            if (StereoPanText != null)
            {
                StereoPanText.text = value.ToString(Common.Util.BaseConstants.FORMAT_TWO_DECIMAL_PLACES);
            }
        }

        #endregion
    }
}
// © 2016-2019 crosstales LLC (https://www.crosstales.com)