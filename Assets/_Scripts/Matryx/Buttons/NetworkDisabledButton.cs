using CalcFlowUI;
using System.Collections;
using System.Collections.Generic;
using UnityEngine;

public class NetworkDisabledButton : MonoBehaviour
{
    enum DisableType
    {
        FlexButton,
        Other
    }

    [SerializeField]
    DisableType disableType;

    private string networkEnableEndpoint = "http://13.57.11.64/v1/ready/?name=";
    // Use this for initialization
    void Start()
    {
        //WebLoader.Instance.Load(networkEnableEndpoint + gameObject.name, ProcessTournaments);
    }

    // Update is called once per frame
    void Update()
    {

    }

    private void ProcessTournaments(string jsonString)
    {
        JSONObject jsonObject = new JSONObject(jsonString);
        jsonObject.GetField("success", delegate (JSONObject results)
        {
            if(!results.b)
            {
                if (disableType == DisableType.FlexButton)
                {
                    GetComponent<FlexButtonComponent>().SetState(-1);
                }
                else
                {
                    //GetComponent<HighlightOnRaycast>().HighlightColor = new Color(100f / 255f, 100f / 255f, 100f / 255f);
                    Destroy(GetComponent<HighlightOnRaycast>());
                    Button[] buttons = GetComponents<Button>();
                    foreach (Button button in buttons)
                    {
                        button.Disable();
                    }
                }
            }
        });
    }
}
