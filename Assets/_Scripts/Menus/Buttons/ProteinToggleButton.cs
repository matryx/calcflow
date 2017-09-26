//using UnityEngine;
//using System.Collections;
//using UniRx;
//using UniRx.Triggers;

//// TODO: play sound effects

//public class ToggleActiveEvent
//{
//    public readonly string name;
//    public readonly bool enabled;

//    public readonly static int NAME = 0;
//    public readonly static int ENABLED = 1;

//    public ToggleActiveEvent(int playerID, string name, bool enabled)
//    {
//        this.name = name;
//        this.enabled = enabled;
//    }
//}

///// <summary>
///// GUI element that is used to instantiate a specific protein
///// </summary>
//public class ProteinToggleButton : MenuButton
//{
//    // Make sure this is set in the Inspector to the correct protein!
//    public GameObject protein;
//    private string proteinName;

//    private static readonly Subject<ToggleActiveEvent> toggleActive = new Subject<ToggleActiveEvent>();
//    public static IObservable<ToggleActiveEvent> ToggleActive { get { return toggleActive; } }

//    CompositeDisposable disposables = new CompositeDisposable();
//    System.IDisposable buttonSync;

//    public void Start()
//    {
//        // All buttons start off disabled until a protein is loaded in
//        GetComponent<BoxCollider>().enabled = false;
//        GetComponent<Renderer>().material.color = disabledColor;
//    }

//    public void OnEnable()
//    {
//        // Subscribe to the buttonsynctrigger which is true when someone becomes presenter and false otherwise
//        // This helps to synchronize the current protein state for this button when presenters are changed
//        buttonSync = PlayerUpdateManager.buttonSyncTrigger
//               .ObserveEveryValueChanged(response => response.Value)
//               .Where(_ => this.protein != null)
//               .Subscribe(turnActive =>
//               {
//                   if (turnActive)
//                   {
//                       if (protein.activeInHierarchy)
//                       {
//                           GetComponent<Renderer>().material.color = selectedColor;
//                       }
//                       else
//                       {
//                           GetComponent<Renderer>().material.color = defaultColor;
//                       }
//                   }
//               });
//        disposables.Add(buttonSync);

//    }

//    public void OnDisable()
//    {
//        //unsubscribe from this event when disabled
//        disposables.Remove(buttonSync);
//    }

//    /// <summary>
//    /// Sets the protein reference and enables the button
//    /// </summary>
//    public void SetProtein(GameObject protein)
//    {
//        this.protein = protein;
//        proteinName = protein.name;
//        transform.parent.GetComponentInChildren<TMPro.TextMeshPro>().text = proteinName;
//        GetComponent<BoxCollider>().enabled = true;
//        GetComponent<Renderer>().material.color = selectedColor;
//    }

//    public override void OnTriggerEnter(Collider other)
//    {
//        base.OnTriggerEnter(other);

//        if (!validCollision)
//            return;

//        if (!protein)
//        {
//            Debug.LogError("Protein not set on button!");
//            return;
//        }

//        // show/hide the protein in the scene
//        if (protein.GetActive())
//        {
//            // Hide the protein
//            GetComponent<Renderer>().material.color = defaultColor;
//            Debug.Log("Hiding protein: " + proteinName);
//            toggleActive.OnNext(new ToggleActiveEvent(PhotonNetwork.player.ID, proteinName, false));
//        }
//        else
//        {
//            // Show the protein
//            GetComponent<Renderer>().material.color = selectedColor;
//            Debug.Log("Showing protein: " + proteinName);
//            toggleActive.OnNext(new ToggleActiveEvent(PhotonNetwork.player.ID, proteinName, true));
//        }
//    }

//    public override void OnTriggerExit(Collider other)
//    {
//        // Purposefully hides the base OnTriggerExit so that we can control when the button light goes off
//    }
//}
