using UnityEngine;
using System.Collections;
using Oculus.Platform;

public class EntitlementCheck
{
    [RuntimeInitializeOnLoadMethod(RuntimeInitializeLoadType.BeforeSceneLoad)]
    static void OnBeforeSceneLoadRuntimeMethod()
    {
        //Core.Initialize("1143046855744783"); // CalcFlow app ID
        Core.Initialize(); // CalcFlow app ID
        Debug.Log("Checking of user has valid entitlement..");
        Entitlements.IsUserEntitledToApplication().OnComplete(
            (Message msg) =>
            {
                if (msg.IsError)
                {
                    // User is NOT entitled.
                    Debug.Log("Error: User is NOT entitled.");
                    UnityEngine.Application.Quit();
                }
                else
                {
                    // User IS entitled
                    Debug.Log("User is entitled. Proceeding as normal..");
                }
            }
        );
    }

}