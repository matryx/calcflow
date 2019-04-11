using UnityEngine;
using UnityEngine.UI;

namespace Crosstales.UI.Util
{
    /// <summary>Controller for audio filters.</summary>
    //[HelpURL("https://www.crosstales.com/media/data/assets/radio/api/class_crosstales_1_1_radio_1_1_demo_1_1_audio_filter_controller.html")]
    public class AudioFilterController : MonoBehaviour
    {
        #region Variables

        [Header("Audio Filters")]
        /// <summary>Searches for all audio filters in the whole scene (default: true).</summary>
        [Tooltip("Searches for all audio filters in the whole scene (default: true).")]
        public bool FindAllAudioFiltersOnStart = true;

        public AudioReverbFilter[] ReverbFilters;
        public AudioChorusFilter[] ChorusFilters;
        public AudioEchoFilter[] EchoFilters;
        public AudioDistortionFilter[] DistortionFilters;
        public AudioLowPassFilter[] LowPassFilters;
        public AudioHighPassFilter[] HighPassFilters;

        [Header("Settings")]
        [Tooltip("Resets all active audio filters (default: on).")]
        public bool ResetAudioFiltersOnStart = true;
        public bool ChorusFilter = false;
        public bool EchoFilter = false;
        public bool DistortionFilter = false;
        public float DistortionFilterValue = 0.5f;
        public bool LowpassFilter = false;
        public float LowpassFilterValue = 5000f;
        public bool HighpassFilter = false;
        public float HighpassFilterValue = 5000f;

        [Header("UI Objects")]
        public Dropdown ReverbFilterDropdown;

        public Text DistortionText;
        public Text LowpassText;
        public Text HighpassText;

        private System.Collections.Generic.List<AudioReverbPreset> reverbPresets = new System.Collections.Generic.List<AudioReverbPreset>();

        private bool initalized = false;

        #endregion


        #region MonoBehaviour methods

        public void Start()
        {
            System.Collections.Generic.List<Dropdown.OptionData> options = new System.Collections.Generic.List<Dropdown.OptionData>();


            foreach (AudioReverbPreset arp in System.Enum.GetValues(typeof(AudioReverbPreset)))
            {
                options.Add(new Dropdown.OptionData(arp.ToString()));

                reverbPresets.Add(arp);
            }

            if (ReverbFilterDropdown != null)
            {
                ReverbFilterDropdown.ClearOptions();
                ReverbFilterDropdown.AddOptions(options);
            }
        }

        public void Update()
        {
            if (!initalized && Time.frameCount % 30 == 0)
            {
                initalized = true;

                if (FindAllAudioFiltersOnStart)
                {
                    FindAllAudioFilters();
                }

                if (ResetAudioFiltersOnStart)
                {
                    ResetAudioFilters();
                }
            }
        }

        #endregion


        #region Public methods

        /// <summary>Finds all audio filters in the scene.</summary>
        public void FindAllAudioFilters()
        {
            ReverbFilters = FindObjectsOfType(typeof(AudioReverbFilter)) as AudioReverbFilter[];
            ChorusFilters = FindObjectsOfType(typeof(AudioChorusFilter)) as AudioChorusFilter[];
            EchoFilters = FindObjectsOfType(typeof(AudioEchoFilter)) as AudioEchoFilter[];
            DistortionFilters = FindObjectsOfType(typeof(AudioDistortionFilter)) as AudioDistortionFilter[];
            LowPassFilters = FindObjectsOfType(typeof(AudioLowPassFilter)) as AudioLowPassFilter[];
            HighPassFilters = FindObjectsOfType(typeof(AudioHighPassFilter)) as AudioHighPassFilter[];
        }

        /// <summary>Resets all audio filters.</summary>
        public void ResetAudioFilters()
        {
            ReverbFilterDropdownChanged(0);
            ChorusFilterEnabled(ChorusFilter);
            EchoFilterEnabled(EchoFilter);
            DistortionFilterEnabled(DistortionFilter);
            DistortionFilterChanged(DistortionFilterValue);
            LowPassFilterEnabled(LowpassFilter);
            LowPassFilterChanged(LowpassFilterValue);
            HighPassFilterEnabled(HighpassFilter);
            HighPassFilterChanged(HighpassFilterValue);
        }

        public void ReverbFilterDropdownChanged(System.Int32 index)
        {
            foreach (AudioReverbFilter reverbFilter in ReverbFilters)
            {
                reverbFilter.reverbPreset = reverbPresets[index];
            }
        }

        public void ChorusFilterEnabled(bool enabled)
        {
            foreach (AudioChorusFilter chorusFilter in ChorusFilters)
            {
                chorusFilter.enabled = enabled;
            }
        }

        public void EchoFilterEnabled(bool enabled)
        {
            foreach (AudioEchoFilter echoFilter in EchoFilters)
            {
                echoFilter.enabled = enabled;
            }
        }

        public void DistortionFilterEnabled(bool enabled)
        {
            foreach (AudioDistortionFilter distortionFilter in DistortionFilters)
            {
                distortionFilter.enabled = enabled;
            }
        }

        public void DistortionFilterChanged(float value)
        {
            foreach (AudioDistortionFilter distortionFilter in DistortionFilters)
            {
                distortionFilter.distortionLevel = value;
            }

            if (DistortionText != null)
            {
                DistortionText.text = value.ToString(Common.Util.BaseConstants.FORMAT_TWO_DECIMAL_PLACES);
            }
        }

        public void LowPassFilterEnabled(bool enabled)
        {
            foreach (AudioLowPassFilter lowPassFilter in LowPassFilters)
            {
                lowPassFilter.enabled = enabled;
            }
        }

        public void LowPassFilterChanged(float value)
        {
            foreach (AudioLowPassFilter lowPassFilter in LowPassFilters)
            {
                lowPassFilter.cutoffFrequency = value;
            }

            if (LowpassText != null)
            {
                LowpassText.text = value.ToString(Common.Util.BaseConstants.FORMAT_NO_DECIMAL_PLACES);
            }
        }

        public void HighPassFilterEnabled(bool enabled)
        {
            foreach (AudioHighPassFilter highPassFilter in HighPassFilters)
            {
                highPassFilter.enabled = enabled;
            }
        }

        public void HighPassFilterChanged(float value)
        {
            foreach (AudioHighPassFilter highPassFilter in HighPassFilters)
            {
                highPassFilter.cutoffFrequency = value;
            }

            if (HighpassText != null)
            {
                HighpassText.text = value.ToString(Common.Util.BaseConstants.FORMAT_NO_DECIMAL_PLACES);
            }
        }

        #endregion
    }
}
// © 2016-2019 crosstales LLC (https://www.crosstales.com)