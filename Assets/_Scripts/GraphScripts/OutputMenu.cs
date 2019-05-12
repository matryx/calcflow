using Matryx;
using System;
using System.Collections;
using System.Collections.Generic;
using System.Linq;
using System.Security.Cryptography;
using System.Text;
using UnityEngine;

public class OutputMenu : MonoBehaviour
{
    [SerializeField]
    SecondaryMenu secondaryMenu;
    [SerializeField]
    CustomParametrizedSurface customParametrizedSurface;

    internal class KeyboardInputResponder : FlexMenu.FlexMenuResponder
    {
        OutputMenu outputMenu;
        internal KeyboardInputResponder(OutputMenu outputMenu)
        {
            this.outputMenu = outputMenu;
        }

        public void Flex_ActionStart(string name, FlexActionableComponent sender, GameObject collider)
        {
            outputMenu.HandleInput(sender);
        }

        public void Flex_ActionEnd(string name, FlexActionableComponent sender, GameObject collider) {}

    }

    //PRESTREAM CODE
    bool saveable = true;
    bool Saveable
    {
        get
        {
            return saveable;
        }
        set
        {
            if (saveable == false && value == true)
            {
                saveButton.SetState(0);
                if (exportButton != null)
                {
                    exportButton.SetState(0);
                }
            }
            else if (value == false)
            {
                saveButton.SetState(-1);
                if (exportButton != null)
                {
                    exportButton.SetState(-1);
                }
            }
            saveable = value;
        }
    }

    FlexActionableComponent saveButton;
    FlexActionableComponent exportButton;

    public FlexMenu menu;

    CalcManager calcManager;

    public void Initialize(CalcManager cm)
    {
        KeyboardInputResponder responder = new KeyboardInputResponder(this);
        menu.RegisterResponder(responder);

        calcManager = cm;
        saveButton = transform.Find("ControlPanel/Save").GetComponent<FlexActionableComponent>();
        exportButton = transform.Find("ControlPanel/GenerateMesh").GetComponent<FlexActionableComponent>();
        if(UnityEngine.SceneManagement.SceneManager.GetActiveScene().name.Equals("3 - FreeParametrization"))
        {
            calcManager.inputHandler += LookupCommitStateByExpressionSet;
        }
    }

    protected void HandleInput(FlexActionableComponent sender)
    {
        switch (sender.name)
        {
            default:
                print("unknown input: " + sender.name);
                break;
            case "Button_Xinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions["X"]);
                break;
            case "Button_Yinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions["Y"]);
                break;
            case "Button_Zinput":
                calcManager.SetOutput(calcManager.expressionSet.expressions["Z"]);
                break;
            case "umin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["u"].Min);
                break;
            case "umax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["u"].Max);
                break;
            case "tmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["t"].Min);
                break;
            case "tmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["t"].Max);
                break;
            case "vmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["v"].Min);
                break;
            case "vmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["v"].Max);
                break;
            case "wmin":
                calcManager.SetOutput(calcManager.expressionSet.ranges["w"].Min);
                break;
            case "wmax":
                calcManager.SetOutput(calcManager.expressionSet.ranges["w"].Max);
                break;
            case "GenerateMesh":
                calcManager.toExport = true;
                break;
            case "Save":
                if (Saveable)
                {
                    calcManager.saveLoadMenu.Save();
                }
                break;
            case "Claim/Commit":
                var button = sender.GetComponent<ClaimCommitButton>();
                var state = button.GetState();
                // if no account active, open up the secondary menu to the matryx tab
                if (NetworkSettings.currentAddress == null)
                {
                    ExpandContract expandContract = secondaryMenu.transform.parent.gameObject.GetComponent<ExpandContract>();
                    StartCoroutine(expandContract.Expand(0.3f,
                        (obj) =>
                        {
                            RayCastButton raycastButton = secondaryMenu.transform.Find("Panel/Matryx/Body").gameObject.GetComponent<RayCastButton>();
                            raycastButton.PressButton(null);
                        }));
                }
                else if (state == ClaimCommitButton.CommitButtonState.YetToBeClaimed)
                {
                    button.SetState(ClaimCommitButton.CommitButtonState.Processing);

                    StartCoroutine(ClaimCommitButton.Instance.commit.claim(
                        (obj) =>
                        {
                            button.SetState(ClaimCommitButton.CommitButtonState.YetToBeCommitted);
                        },
                        (nada) =>
                        {
                            button.SetState(ClaimCommitButton.CommitButtonState.CantCommit);
                        })
                    );
                }
                else if (state == ClaimCommitButton.CommitButtonState.YetToBeCommitted)
                {
                    button.SetState(ClaimCommitButton.CommitButtonState.Processing);

                    StartCoroutine(ClaimCommitButton.Instance.commit.create(
                        (obj) =>
                        {
                            button.SetState(ClaimCommitButton.CommitButtonState.Committed);
                        },
                        (nada) =>
                        {
                            button.SetState(ClaimCommitButton.CommitButtonState.CantCommit);
                        })
                    );
                }
                break;
        }

        calcManager.manageText();
    }

    public static string lastSurface = "";
    private void LookupCommitStateByExpressionSet(object sender, EventArgs args)
    {
        ClaimCommitButton.Instance.SetState(ClaimCommitButton.CommitButtonState.Disabled);

        var surface = SerializeSurface();
        if (lastSurface.Equals(surface)) return;
        if (!MatryxCommit.storageClaimsLoaded) return;

        List<string> fileNames = new List<string> { "jsonContent.json" };
        List<byte[]> contents = new List<byte[]> { MatryxCortex.serializer.Serialize(surface) };
        List<string> fileTypes = new List<string> { "application/json" };
        // get ipfs hash
        var ipfsHashRequest = new Utils.CoroutineWithData<string>(MatryxCortex.Instance, MatryxCortex.uploadFiles(fileNames, contents, fileTypes, "&only-hash=true",
            (object multiHash) =>
            {
                // This correct? You should look at commits that've already been created and see if there is one with this hash
                string ipfsHash = multiHash as string;

                bool commitExists = MatryxCommit.commits.ContainsKey(ipfsHash);
                bool claimExists = MatryxCommit.claims.ContainsKey(ipfsHash);

                if (claimExists && !commitExists)
                {
                    ClaimCommitButton.Instance.commit = new MatryxCommit(ipfsHash, surface);
                    ClaimCommitButton.Instance.SetState(ClaimCommitButton.CommitButtonState.YetToBeCommitted);
                }
                else if (commitExists)
                {
                    if (MatryxCommit.commits[ipfsHash].mine)
                    {
                        ClaimCommitButton.Instance.SetState(ClaimCommitButton.CommitButtonState.Committed);
                    }
                    else
                    {
                        ClaimCommitButton.Instance.SetState(ClaimCommitButton.CommitButtonState.CantCommit);
                    }

                    ClaimCommitButton.Instance.commit = MatryxCommit.commits[ipfsHash];
                }
                else
                {
                    ClaimCommitButton.Instance.commit = new MatryxCommit(ipfsHash, surface);
                    ClaimCommitButton.Instance.SetState(ClaimCommitButton.CommitButtonState.YetToBeClaimed);
                }
            }));
    }

    private string SerializeSurface()
    {
        List<SerializableExpressionSet> serializableExpressions = calcManager.paramSurface.expressionSets.Select(x => new SerializableExpressionSet(x)).ToList();
        return JsonHelper.ToJson(serializableExpressions);
    }

    private void Update()
    {
        Saveable = !calcManager.paramSurface.isGraphing();
    }

}
